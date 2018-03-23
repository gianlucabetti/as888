000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400130610      * RMN ed RMA sono, come la parte fissa dei "CHI SONO", gli ultimi 15 caratteri
000500130610      * a dx del valore inviato dal cliente;
000600130611      * i "CHI SONO" si sviluppano dal nr.colli poi gli si aggiunge il check digit E10;
000700130611      * peso e volume vanno arrotondati;
000800130611      *
000900121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001000120209     F*
001100990910     Ftivin00r  uF   E             DISK    usropn
001200100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001300040119     FFIVATwwr  O    E             DISK    usropn
001400000313     D*
001500040113     D*------------
001600040113     D* SCHIEDE A PROGRAMMA
001700040113     D*------------
001800080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001900010330     D*----------------------------------------------------
002000010330     D* DICHIARAZIOINE VARIABILI DI WRK
002100010330     D*----------------------------------------------------
002200010330     D dscmz         e ds                  inz
002300010330     D psds           sds
002400010330     D  procname         *PROC
002500010330     D tivlrds       e ds                  extname(tivlr00f)
002600040128     D tisi95ds      e ds
002700100413     D fivabds       e ds                  extname(fivab00f)
002800100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002900100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003000010330     D esito           s              1
003100010330     D prmlit          s             10
003200010330     D prmfir          s             10
003300010330     D wrkesito        s                   like(esito)
003400010330     D rrnum           s              6  0 INZ(*zeros)
003500040510     D parccm          s              8    INZ(*blanks)
003600040119     D parmbr          s             10    INZ(*blanks)
003700040119     D paropz          s              1    INZ(*blanks)
003800040119     D chkcall         s              1    INZ(*blanks)
003900080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004000080418     D wFlgCAS         s              1    INZ(*blanks)
004100120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004200120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004300130528     D Intestazioni    s               n   inz(*off)
004400100326     D*------------------
004500100326     D* DS REPERIMENTO NUMERATORE
004600100326     D*------------------
004700100326     D trul33ds      e ds                  inz
004800100326     D*------------------
004900100326     D* DS ARCHITETTURA
005000100326     D*------------------
005100100326     D kpjba         e ds                  inz
005200130611     D*------------------
005300130611     D TRUL28DSX     e ds                  inz
005400100326
005500030715     D*------------------
005600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005700030715     D*------------------
005800030715     D SkSplitFLD      S             10    DIM(100)
005900030715     D SkSplitCSV      S            256    DIM(100)
006000030715     D CharCSV         S              1
006100030715     D CharTXT         S              1
006200030715     D CharNUM         S              1
006300100318     D CharSOS         S              1
006400030715     D posDa           S              3  0 INZ(*zeros)
006500030715     D posA            S              3  0 INZ(*zeros)
006600030715     D i               s              3  0 INZ(1)
006700030715     D wGiro           s              1  0 INZ(*zeros)
006800070823     D wVATNOT_A       s                   LIKE(VATNOT)
006900060331     D wVATNOT_B       s                   LIKE(VATNOT)
007000060331     D wVATNOT_E       s                   LIKE(VATNOT)
007100110927     D wVATNOT_I       s                   LIKE(VATNOT)
007200131129     D wVATNOT_J       s                   LIKE(VATNOT)
007300131129     D wVATNOT_IJ      s             70
007400131129     D wNOT            s             70
007500120411     D Data8A          s              8
007600030822     D*------------------
007700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007800030822     D*------------------
007900030822     D posDaDft        S              3  0 INZ(*zeros)
008000030822     D posADft         S              3  0 INZ(*zeros)
008100030822     D j               s              3  0 INZ(1)
008200030822     D wGiroDft        s              1  0 INZ(*zeros)
008300080418     D*------------------
008400080418     D* Costanti
008500080418     D*------------------
008600080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008700080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008800100318
008900100318     D*------------------
009000100318     D* LINKING A DEFINIZIONI ESTERNE
009100100318     D*------------------
009200100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009300100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009400100318
009500010330
009600010330
009700010330
009800000913     C                   reset                   rrnum
009900990921     C                   reset                   esito
010000990921     C                   reset                   wrkesito
010100010601     C*
010200010601     C                   exsr      opeini
010300010605     C                   exsr      rwvab
010400010601     C*
010500010601     C                   seton                                        lr
010600010601
010700010601
010800010601
010900010601     C*--------------------------------------------------------
011000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011100010601     C*--------------------------------------------------------
011200010601     C     PREELA        BEGSR
011300010601     C*
011400010601     C* SE OCCORRE SPEDIRE IN FILIALE
011500010601     C                   if        invfil <> *zeros and
011600010601     C                             flgGiro = '0'
011700010601     C*
011800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011900010601     C                   eval      flgGiro = '1'
012000010601     C*
012100010601     C                   endif
012200010601     C*
012300010601     C                   ENDSR
012400010601     C***
012500010601
012600010601
012700010601
012800010601
012900010601     C*--------------------------------------------------------
013000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013100010601     C*--------------------------------------------------------
013200010601     C     ENDELA        BEGSR
013300040119     C*
013400040119     C* Effettuo la chiamata al CLLE preposto
013500040119     C                   call(e)   'TITVVTC'
013600040510     C                   parm                    parccm
013700040119     C                   parm                    parmbr
013800040119     C                   parm      '2'           paropz
013900050201     C*
014000050201     C* Effettuo lancio TISI95 solo x chiusura
014100050201     C                   CLEAR                   TISI95DS
014200050201     C                   EVAL      I95TLA = 'C'
014300050201     C                   CALL      'TISI95R'
014400050201     C                   PARM                    TISI95DS
014500000616     C*
014600010601     C                   ENDSR
014700010601
014800010601
014900010330     C*--------------------------------------------------------
015000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015100010330     C*--------------------------------------------------------
015200010605     C     RWVAB         BEGSR
015300010330     C*
015400010330     C                   if        not %open(tivin00r)
015500010330     C                   open      tivin00r
015600010330     C                   endif
015700021025     C                   if        not %open(fivabwwr)
015800021025     C                   open      fivabwwr
015900010330     C                   endif
016000040119     C*
016100040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016200040119     C                   exsr      prevat
016300040119     C*
016400040119     C                   if        chkcall = '0'
016500040119     C*
016600040119     C                   if        not %open(fivatwwr)
016700040119     C                   open      fivatwwr
016800040119     C                   endif
016900010330     C*
017000040119     C                   clear                   §CTROKVB          7 0
017100040119     C                   clear                   §CTROKVT          7 0
017200040119     C                   clear                   §CTRMO            7 0
017300040119     C                   clear                   §CTRNO            7 0
017400120410     C*
017500120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017600120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017700120410     C                   IF        Intestazioni = *off
017800120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017900120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018000120410     C                   exsr      inzvar
018100120410     C                   exsr      impvab
018200120410     C                   ENDIF
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
021800070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021900130605     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
022000130605     C* evitare che vengano elisi i blank
022100130610     C                   eval      vindta  = ' ' + %trim(vindta)
022200070925     C*
022300080527     C                   exsr      inzvar
022400100318     C                   exsr      normCSV
022500010605     C                   exsr      impvab
022600010601     C*
022700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022800010605     C                   eval      depfil = VABLNP
022900010601     C                   exsr      repfil
023000010601     C                   if        depfil = invfil
023100021025     C                   if        vlrpoi = 999
023200030822     C                   move(P)   invfil        VABFGS
023300021025     C                   else
023400030822     C                   move(P)   vlrpoi        VABFGS
023500021025     C                   endif
023600010601     C*
023700010601     C                   exsr      PREELA
023800010601     C*
023900010604     C* Ebbene...
024000010604     C*
024100040119     C  N31              add       1             §CTROKVB
024200040119     C   32              add       1             §CTRMO
024300040119     C   31              add       1             §CTRNO
024400080418     C*
024500030822     C                   if        wGiro = 2
024600080513     C* Scrivo sempre il VAT
024700080513     C  N31              exsr      wrivat
024800110201     C                   exsr      chkvab
024900080513     C                   endif
025000010604     C*
025100010604     C                   if        *in31 = *off and
025200010604     C                             *in32 = *off
025300010604     C                   eval      vinflg = '1'
025400010604     C                   else
025500010604     C                   eval      vinflg = '2'
025600010604     C                   endif
025700010604     C                   endif
025800010601     C*
025900010604     C                   endif
026000010604     C*
026100010330     C                   else
026200010330     C                   eval      vinflg = '1'
026300010330     C                   endif
026400010601     C*
026500010601     C  N70              update    tivin000
026600010330     C*
026700030822     C  N70              enddo
026800040119     C*
026900080527     C* Verifico dati rimasti "in cannna"...
027000080527     C                   seton                                        65
027100110201     C                   exsr      chkvab
027200080527     C*
027300040119     C                   endif                                                  (endif - chkcall)
027400010601     C*
027500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027600010601     C                   if        cntNonEl = *zeros or
027700010601     C                             flgMulti = '0'
027800010330     C* Se non ci sono record con errori ...
027900010601     C                   if        §ctrno = 0 and
028000010604     C                             §ctrmo = 0 and
028100010601     C                             flgStato <> '2'
028200010330     C* ... restituisco esito OK.
028300010330     C                   eval      wrkesito = '0'
028400010330     C                   else
028500040119     C                   if        §ctrokvb > 0
028600010330     C                   eval      wrkesito = '1'
028700010330     C                   else
028800010615     C                   if        flgOk = '0'
028900010615     C                   eval      wrkesito = '2'
029000010615     C                   else
029100010615     C                   eval      wrkesito = '6'
029200010615     C                   endif
029300010330     C                   endif
029400010330     C                   endif
029500010601     C                   else
029600010601     C                   eval      wrkesito = '9'
029700010601     C                   endif
029800010330     C*
029900010330     C                   if        %open(tivin00r)
030000010330     C                   close     tivin00r
030100010330     C                   endif
030200021025     C                   if        %open(fivabwwr)
030300021025     C                   close     fivabwwr
030400010330     C                   endif
030500040512     C                   if        %open(fivatwwr)
030600040512     C                   close     fivatwwr
030700040512     C                   endif
030800010601     C*
030900010601     C                   if        vlrpoi <> 999
031000010601     C                   eval      invfil = vlrpoi
031100010601     C                   endif
031200010330     C*
031300040119     C                   if        §ctrokvb > 0
031400010601     C                             and invfil > *zeros
031500010330     C                   exsr      invio
031600010330     C                   endif
031700010601     C*
031800010618     C                   if        flgGiro = '1'
031900010601     C                   exsr      endela
032000010618     C                   endif
032100010330     C*
032200010330     C                   ENDSR
032300010330     C***
032400080527
032500080527
032600080527
032700080527     C*----------------------------------------------------*
032800080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032900080527     C*----------------------------------------------------*
033000100413     C     CHKVAB        BEGSR
033100110201     C*
033200110201     C* Se richiesti ignorare colli / peso / volume
033300110315 xxx C***                if        *in35
033400110315 xxx C***                z-add     *zeros        VABNCL
033500110315 xxx C***                z-add     *zeros        VABPKB
033600110315 xxx C***                z-add     *zeros        VABVLB
033700110315 xxx C***                endif
033800080527     C*
033900080527     C* Scrivo il VAB solo a rottura d spedizione
034000080527     C                   if        depspe = *zeros
034100080527     C  N31              eval      fivabds_sav = fivabds
034200080527     C                   eval      depspe = vabNSP
034300080527     C                   else
034400080527     C                   if        depspe = vabNSP AND *in65 = *off
034500100413     C   51              add       VABNCL        sav_VABNCL
034600100413     C   51              add       VABPKB        sav_VABPKB
034700100413     C   51              add       VABVLB        sav_VABVLB
034800100720     C   52              z-add     VABNCL        sav_VABNCL
034900100720     C   52              z-add     VABPKB        sav_VABPKB
035000100720     C   52              z-add     VABVLB        sav_VABVLB
035100100720     C   53              add       VABNCL        sav_VABNCL
035200100720     C   53              z-add     VABPKB        sav_VABPKB
035300100720     C   53              z-add     VABVLB        sav_VABVLB
035400080527     C                   else
035500080527     C  N31              eval      fivabds_ok = fivabds_sav
035600080527     C  N31              exsr      wrivab
035700080527     C  N31              eval      fivabds_sav = fivabds
035800080527     C                   eval      depspe = vabNSP
035900080527     C                   endif
036000080527     C                   endif
036100080527     C*
036200080527     C                   ENDSR
036300040119
036400040119
036500040119
036600040119     C*----------------------------------------------------*
036700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036800040119     C*----------------------------------------------------*
036900040510     C     WRIVAB        BEGSR
037000080513     C*
037100080513     C* Gestisco l'eventuale rottura x numero spedizione
037200080527     C                   if        VABNSP <> depspe or *in65 = *on
037300070301     C                   if        wVATNOT_E <> *blanks
037400130610     C                   eval      ok_VABCTM = '7Q'
037500070301     C                   endif
037600100413     C                   write     fivab000
037700080513     C                   endif
037800040119     C*
037900040119     C                   ENDSR
038000040119
038100040119
038200040119
038300040119     C*----------------------------------------------------*
038400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038500040119     C*----------------------------------------------------*
038600040510     C     WRIVAT        BEGSR
038700060512     C*
038800060512     C* Inizializzo i campi chiave
038900060512     C                   eval      VATFGS = VABFGS
039000060512     C                   eval      VATAAS = VABAAS
039100060512     C                   eval      VATCCM = VABCCM
039200070823     C                   eval      VATNRS = VABNRS
039300060512     C                   eval      VATNSP = VABNSP
039400060512     C                   eval      VATLNP = VABLNP
039500040119     C*
039600040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039700040510     C                   if        VATTRC = *blanks
039800040510     C                   eval      VATTRC = 'E'
039900040510     C                   endif
040000110318     C                   if        VATNOT <> *blanks
040100040510     C                   write     FIVAT000
040200040119     C                   add       1             §CTROKVT
040300110318     C                   endif
040400060331     C*
040500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040600060331     C                   if        wVATNOT_A <> *blanks
040700060331     C                   eval      VATNOT = wVATNOT_A
040800060331     C                   eval      VATTRC = 'A'
040900060331     C                   write     FIVAT000
041000060331     C                   add       1             §CTROKVT
041100060331     C                   endif
041200060331     C*
041300060331     C                   if        wVATNOT_B <> *blanks
041400060331     C                   eval      VATNOT = wVATNOT_B
041500060331     C                   eval      VATTRC = 'B'
041600060331     C                   write     FIVAT000
041700060331     C                   add       1             §CTROKVT
041800060331     C                   endif
041900060331     C*
042000060331     C                   if        wVATNOT_E <> *blanks
042100110201     C                   eval      VATNOT = wVATNOT_E
042200060331     C                   eval      VATTRC = 'E'
042300120411     C*
042400120411     C* Sviluppati i "CHI SONO" in relazione al numero colli
042500120411     C*
042600120411     C     1             DO        VABNCL        wNumCollo         3 0
042700130611     C*** da analisi del POC il progressivo colli è max 3 char e deve contenere gli zeri non
042800120411     C*** significativi
042900120411     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
043000120411     C                                %editc(wNumCollo:'X')
043100130611     C* recupero il check digit
043200130611     C                   clear                   TRUL28DSX
043300130611     C                   eval      I284STD = 'E10'
043400130611     C                   eval      I284COD = VATNOT
043500130611     C                   call      'TRUL28R4'
043600130611     C                   parm                    TRUL28DSX
043700130611     C* Se tutto ok scrivo il "chi sono" corrente col check digit
043800130611     C                   IF        O284err = *blanks
043900130611     C                   EVAL      VATNOT = %trim(VATNOT)
044000130611     C                              + %trim(O284CkD)
044100120410     C                   write     FIVAT000
044200120410     C                   add       1             §CTROKVT
044300130611     C                   ELSE
044400130611     C                   SETON                                        31
044500130611     C                   EVAL      VATNOT = *blank
044600130611     C                   EVAL      vinmsg = %trimr(vinmsg)
044700130611     C                             + ' ' + 'VATNOT'
044800130611     C                   ENDIF
044900120411     C                   ENDDO
045000120320     C*
045100120320     C                   endif
045200120320     C*
045300110927     C*
045400110927     C                   if        wVATNOT_I <> *blanks
045500110927     C                   eval      VATNOT = wVATNOT_I
045600110927     C                   eval      VATTRC = 'I'
045700110927     C                   write     FIVAT000
045800110927     C                   add       1             §CTROKVT
045900110927     C                   endif
046000131129     C*
046100131129     C                   if        wVATNOT_J <> *blanks
046200131129     C                   eval      VATNOT = wVATNOT_J
046300131129     C                   eval      VATTRC = 'J'
046400131129     C                   write     FIVAT000
046500131129     C                   add       1             §CTROKVT
046600131129     C                   endif
046700040119     C*
046800040119     C                   ENDSR
046900040119
047000040119
047100040119
047200040119     C*----------------------------------------------------*
047300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047400040119     C*----------------------------------------------------*
047500040119     C     PREVAT        BEGSR
047600040119     C*
047700040119     C* Compongo il nome del membro da dare al FIVATWWR
047800040119     C                   eval      parmbr = vlrhdl
047900040119     C                   movel     'M'           parmbr
048000040510     C                   eval      parccm = vlrksc
048100040119     C                   eval      paropz = '1'
048200040119     C* Effettuo la chiamata al CLLE preposto
048300040119     C                   call(e)   'TITVVTC'
048400040119     C                   parm                    parccm
048500040119     C                   parm                    parmbr
048600040119     C                   parm                    paropz
048700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048800040119     C                   if        %error
048900040119     C                   movel     '1'           chkcall
049000040119     C                   else
049100040119     C                   movel     '0'           chkcall
049200040119     C                   endif
049300040119     C*
049400040119     C                   ENDSR
049500010601
049600010601
049700010601
049800010330     C*----------------------------------------------------*
049900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050000010330     C*----------------------------------------------------*
050100040119     C     INZVAR        BEGSR
050200080527     C*
050300080527     C                   setoff                                       65
050400010330     C*
050500030822     C* Inizializzo variabili di wrk
050600020204     C                   Z-ADD     *zeros        Num5_0            5 0
050700101018     C                   MOVEL     *blanks       wFlgCAS
050800060331     C                   MOVEL     *blanks       wVATNOT_A
050900060331     C                   MOVEL     *blanks       wVATNOT_B
051000060331     C                   MOVEL     *blanks       wVATNOT_E
051100110927     C                   MOVEL     *blanks       wVATNOT_I
051200131129     C                   MOVEL     *blanks       wVATNOT_J
051300131129     C                   MOVEL     *blanks       wVATNOT_IJ
051400120411     C                   MOVEL     *blanks       wNOT
051500120411     C                   MOVEL     *blanks       Data8A
051600030822     C*
051700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051800030822     C                   CLEAR                   SkSplitCSV
051900030822     C                   EVAL      i = 1
052000030822     C                   EVAL      posDa = *zeros
052100030822     C                   EVAL      posA  = *zeros
052200080527     C*
052300080527     C                   CLEAR                   FIVABDS
052400080527     C                   CLEAR                   FIVAT000
052500030822     C*
052600030822     C* Reimposto i valori di default
052700030822     C                   EXSR      DEFCAM
052800030822     C*
052900010330     C                   ENDSR
053000010330     C*----------------------------------------------------*
053100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053200010330     C*----------------------------------------------------*
053300010330     C     DEFCAM        BEGSR
053400030822     C*
053500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
053600030715     C* e delimitatore testo.
053700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054000100318     C*
054100100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
054200100318     C                   EVAL      CharSOS = CharNUM
054300030822     C*
054400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
054500100720     C                   SETOFF                                       505152
054600100720     C                   SETOFF                                       53
054700100413     C                   SELECT
054800100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
054900100720     C                   SETON                                        50
055000100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
055100100413     C                   SETON                                        51
055200100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
055300100720     C                   SETON                                        52
055400100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
055500100720     C                   SETON                                        53
055600100413     C                   ENDSL
055700030822     C*
055800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055900030822     C                   EVAL      posDaDft = 1
056000030822     C                   EVAL      posADft  = 0
056100030822     C                   EVAL      wGiroDft = 0
056200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056300030822     C                             posDaDft > 0
056400030822     C*
056500030822     C* Gestisco il 1° giro
056600030822     C                   IF        wGiroDft = 0
056700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056900030822     C* Incremento il contatore dei "giri"
057000030822     C                   EVAL      wGiroDft = 1
057100030822     C                   ELSE
057200030822     C                   EVAL      posDaDft = posADft
057300030822     C                   ENDIF
057400030822     C* Eseguo lo scan x trovare la fine del campo corrente
057500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
057600030822     C*
057700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057900030822     C                   IF        posDaDft > 0 AND
058000030822     C                             posADft  > 0
058100030822     C* NCL
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'NCL'
058600030822     C                   EVAL      PiStr=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   EXSR      CHKNUM
059000030822     C                   IF        PiInt=*on
059100030822     C                   Z-ADD     PiVal         VABNCL
059200030822     C                   ENDIF
059300030822     C                   ENDIF
059400030822     C* CCM
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'CCM'
059900030822     C                   EVAL      PiStr=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   EXSR      CHKNUM
060300030822     C                   IF        PiInt=*on
060400030822     C                   Z-ADD     PiVal         VABCCM
060500030822     C                   ENDIF
060600030822     C                   ENDIF
060700030822     C* LNP
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'LNP'
061200030822     C                   EVAL      PiStr=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   EXSR      CHKNUM
061600030822     C                   IF        PiInt=*on
061700030822     C                   Z-ADD     PiVal         VABLNP
061800030822     C                   ENDIF
061900030822     C                   ENDIF
062000030822     C* NRS
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'NRS'
062500030822     C                   EVAL      PiStr=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   EXSR      CHKNUM
062900030822     C                   IF        PiInt=*on
063000030822     C                   Z-ADD     PiVal         VABNRS
063100030822     C                   ENDIF
063200030822     C                   ENDIF
063300030822     C* CTR
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'CTR'
063800030822     C                   EVAL      PiStr=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   EXSR      CHKNUM
064200030822     C                   IF        PiInt=*on
064300030822     C                   Z-ADD     PiVal         VABCTR
064400030822     C                   ENDIF
064500030822     C                   ENDIF
064600030822     C* PKB
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'PKB'
065100030822     C                   EVAL      PiStr=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   EXSR      CHKNUM
065500030822     C                   IF        PiNum=*on
065600130610     C                   Z-ADD     PiVal         VABPKB
065700030822     C                   ENDIF
065800030822     C                   ENDIF
065900030822     C* VLB
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'VLB'
066400030822     C                   EVAL      PiStr=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   EXSR      CHKNUM
066800030822     C                   IF        PiNum=*on
066900130610     C                   Z-ADD     PiVal         VABVLB
067000030822     C                   ENDIF
067100030822     C                   ENDIF
067200030822     C* QFT
067300030822     C                   IF        %subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):1:3)
067600030822     C                             = 'QFT'
067700030822     C                   EVAL      PiStr=%trim(%subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068000030822     C                   EXSR      CHKNUM
068100030822     C                   IF        PiNum=*on
068200030822     C                   Z-ADD     PiVal         VABQFT
068300030822     C                   ENDIF
068400030822     C                   ENDIF
068500030822     C* CBO
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'CBO'
069000030822     C                   EVAL      VABCBO=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400030822     C* TSP
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'TSP'
069900030822     C                   EVAL      VABTSP=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* VAS
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'VAS'
070800030822     C                   EVAL      VABVAS=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* VCA
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'VCA'
071700030822     C                   EVAL      VABVCA=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100070925     C* TIC
072200070925     C                   IF        %subst(
072300070925     C                             %subst(vlrppt:posDaDft+1:
072400070925     C                             posADft-posDaDft-1):1:3)
072500070925     C                             = 'TIC'
072600070925     C                   EVAL      VABTIC=%trim(%subst(
072700070925     C                             %subst(vlrppt:posDaDft+1:
072800070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900070925     C                   ENDIF
073000030822     C* GCA
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'GCA'
073500030822     C                   EVAL      VABGCA=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* CTM
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'CTM'
074400030822     C                   EVAL      VABCTM=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* FFD
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'FFD'
075300030822     C                   EVAL      VABFFD=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* VAD
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'VAD'
076200030822     C                   EVAL      VABVAD=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* GMA
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'GMA'
077100030822     C                   EVAL      VABGMA=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500030822     C* GGA
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'GGA'
078000030822     C                   EVAL      VABGGA=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400030822     C* GVA
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'GVA'
078900030822     C                   EVAL      VABGVA=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300030822     C* TC1
079400030822     C                   IF        %subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):1:3)
079700030822     C                             = 'TC1'
079800030822     C                   EVAL      VABTC1=%trim(%subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100030822     C                   ENDIF
080200030822     C* TC2
080300030822     C                   IF        %subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):1:3)
080600030822     C                             = 'TC2'
080700030822     C                   EVAL      VABTC2=%trim(%subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081000030822     C                   ENDIF
081100040714     C* VATTRC
081200040714     C                   IF        %subst(
081300040714     C                             %subst(vlrppt:posDaDft+1:
081400040714     C                             posADft-posDaDft-1):1:3)
081500040714     C                             = 'TRC'
081600040714     C                   EVAL      VATTRC=%trim(%subst(
081700040714     C                             %subst(vlrppt:posDaDft+1:
081800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081900040714     C                   ENDIF
082000030822     C* ...
082100030822     C                   ENDIF
082200030822     C                   ENDDO
082300020204     C*
082400010330     C                   ENDSR
082500010607     C*----------------------------------------------------*
082600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
082700010607     C*----------------------------------------------------*
082800010607     C     IMPVAB        BEGSR
082900010607     C*
083000010607     C                   SETOFF                                       3132
083100110315 xxx C***                SETON                                        35
083200030515     C*
083300010607     C                   MOVEL     datcor        VABAAS
083400010607     C                   MOVE      datcor        VABMGS
083500010607     C*
083600030715     C**********
083700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083800030715     C**********
083900030715     C                   IF        wGiro = *zeros
084000040113     C*
084100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084300080429     C                             %trim(intcol(3))+%trim(intcol(4))+
084400080429     C                             %trim(intcol(5))<>*blanks
084500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
084600080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
084700080429     C                                      %trim(intcol(5))
084800070823     C     ';':CharCSV   XLATE     vindta        vindta
084900040510     C                   ENDIF
085000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085100031201     C     minu:maiu     XLATE     vindta        vindta
085200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085300030715     C                   DOW       posDa <= %len(%trim(vindta))
085400030715     C*
085500030715     C* Gestisco il 1° campo
085600030715     C                   IF        i = 1
085700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085800030715     C                   EVAL      posDa = 1
085900030715     C* Eseguo lo scan x trovare la fine del primo campo
086000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086100030715     C* A questo punto "estraggo" il campo corrente
086200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086300030715     C                                                    (posA-posDa))
086400030715     C* X i campi successivi al 1°
086500030715     C                   ELSE
086600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086700030715     C                   EVAL      posDa = posA + 1
086800030715     C* Eseguo lo scan x trovare la fine del campo corrente
086900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087000030715     C* Gestisco l'ultimo campo
087100030715     C                   IF        posA = *zeros
087200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087300030715     C* A questo punto "estraggo" il campo corrente
087400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087500030715     C                                                    (posA-posDa))
087600030715     C                   LEAVE
087700030715     C                   ELSE
087800030715     C* A questo punto "estraggo" il campo corrente
087900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088000030715     C                                                    (posA-posDa))
088100030715     C                   ENDIF
088200030715     C                   ENDIF
088300030715     C* Incremento il contatore d campo
088400030715     C                   EVAL      i = i +1
088500030715     C                   ENDDO
088600030715     C*
088700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088800030715     C* delimitazione testo
088900030715     C                   EVAL      i = 1
089000030715     C                   DOW       i < %elem(SkSplitFLD)
089100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089300030715     C                   EVAL      i = i + 1
089400030715     C                   ENDDO
089500030715     C                   EVAL      wGiro = 1
089600030715     C                   ELSE
089700030715     C                   EVAL      wGiro = 2
089800031204     C**********
089900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090000031204     C* d separatore campo
090100031204     C**********
090200031204     C                   EVAL      vindta = %trim(vindta)
090300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090400031204     C                   EVAL      vindta = %subst(vindta:2)
090500031204     C                   ENDDO
090600031204     C*
090700031204     C                   Z-ADD     *zeros        lunghInput        4 0
090800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091200110912     C                   ENDDO
091300030715     C**********
091400030715     C* Effettuo lo split del campo dati d input
091500030715     C**********
091600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091700030715     C                   DOW       posDa <= %len(%trim(vindta))
091800030715     C*
091900030715     C* Gestisco il 1° campo
092000030715     C                   IF        i = 1
092100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092200030715     C                   EVAL      posDa = 1
092300030715     C* Eseguo lo scan x trovare la fine del primo campo
092400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092500030715     C* A questo punto "estraggo" il campo corrente
092600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092700030715     C                                                    (posA-posDa))
092800030715     C* X i campi successivi al 1°
092900030715     C                   ELSE
093000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093100030715     C                   EVAL      posDa = posA + 1
093200031204     C* Verifico che nn vi sia il campo nn valorizzato
093300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093400031204     C* Se campo nn valorizzato skippo al prossimo
093500031204     C                   EVAL      SkSplitCSV(i) = *blanks
093600031204     C                   EVAL      posA  = posA + 1
093700031204     C                   ELSE
093800030715     C* Eseguo lo scan x trovare la fine del campo corrente
093900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094000030715     C* Gestisco l'ultimo campo
094100030715     C                   IF        posA = *zeros
094200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094300030715     C* A questo punto "estraggo" il campo corrente
094400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094500030715     C                                                    (posA-posDa))
094600030715     C                   LEAVE
094700030715     C                   ELSE
094800030715     C* A questo punto "estraggo" il campo corrente
094900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095000030715     C                                                    (posA-posDa))
095100030715     C                   ENDIF
095200031204     C                   ENDIF
095300030715     C                   ENDIF
095400030715     C* Incremento il contatore d campo
095500030715     C                   EVAL      i = i +1
095600030715     C                   ENDDO
095700030715     C*
095800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095900030715     C* delimitazione testo
096000030715     C                   EVAL      i = 1
096100030715     C                   DOW       i < %elem(SkSplitCSV)
096200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096400030715     C                   EVAL      i = i + 1
096500030715     C                   ENDDO
096600100720     C*
096700100720     C* Stacco un progressivo x numero spedizione
096800130610 xxx C                   EXSR      repNSP
096900100326     C*
097000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097100030715     C                   EVAL      i = 1
097200030715     C                   DOW       i < %elem(SkSplitFLD)
097300030715     C*
097400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097600030715     C                             SkSplitCSV(i) <> *blanks
097700030721     C*** ATB
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097900030721     C                   EVAL      VABATB = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030715     C*** RSD
098200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098600030715     C* ==
098700130528     C                   ENDIF
098800030721     C*** RD2
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030715     C*** IND
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099400030715     C                   EVAL      VABIND = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030715     C*** LOD
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030715     C*** PRD
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030721     C*** NZD
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030715     C*** RMA
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** NOT
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030715     C*** NT2
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
101900030715     C                   ENDIF
102000030715     C*** VCA
102100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102300030715     C                   ENDIF
102400030715     C*** VAS
102500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102700030715     C                   ENDIF
102800030715     C*** FFD
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030721     C*** GC1
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** GC2
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** TSP
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104300030721     C                   ENDIF
104400040510     C*** CBO
104500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
104700040510     C                   ENDIF
104800030721     C*** NAS
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** TIC
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** GCA
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
105800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** XCO
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** CTM
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
106600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** TCR
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** CTS
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** FTM
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
107800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** VAD
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** GMA
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
108600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** GGA
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** GVA
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** TC1
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
109800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** TC2
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** SCL
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
110600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** RMO
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** NMO
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*
111700030721     C* Reperisco quindi i campi numerici...
111800040510     C*** NSP
111900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112000040510     C                   EVAL      PiStr=SkSplitCSV(i)
112100040510     C                   EXSR      CHKNUM
112200040510     C                   IF        PiInt=*on
112300040510     C                   Z-ADD     PiVal         VABNSP
112400040510     C                   ELSE
112500040510     C                   SETON                                        31
112600040510     C                   EVAL      VABNSP = *zeros
112700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
112800040510     C                             + ' ' + 'VABNSP'
112900040510     C                   ENDIF
113000040510     C                   ENDIF
113100071106     C*** RMN
113200071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113300071106     C                   EVAL      PiStr=SkSplitCSV(i)
113400071106     C                   EXSR      CHKNUM
113500071106     C                   IF        PiInt=*on
113600071106     C                   Z-ADD     PiVal         VABRMN
113700071106     C                   ELSE
113800071106     C                   SETON                                        32
113900071106     C                   EVAL      VABRMN = *zeros
114000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114100071106     C                             + ' ' + 'VABRMN'
114200071106     C                   ENDIF
114300071106     C                   ENDIF
114400030721     C*** CAD
114500130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
114600130610     C                   EVAL      PiStr=SkSplitCSV(i)
114700130610     C                   EXSR      CHKNUM
114800130610     C                   IF        PiInt=*on
114900130610     C                   Z-ADD     PiVal         Num5_0
115000130610     C                   MOVEL(p)  Num5_0        VABCAD
115100130610     C                   ELSE
115200130610     C                   SETON                                        32
115300130610     C                   EVAL      VABCAD = *zeros
115400130610     C                   EVAL      vinmsg = %trimr(vinmsg)
115500130610     C                             + ' ' + 'VABCAD'
115600130610     C                   ENDIF
115700130605     C                   ENDIF
115800030721     C*** NCL
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiInt=*on
116300030721     C                   Z-ADD     PiVal         VABNCL
116400030721     C                   ELSE
116500030721     C                   SETON                                        32
116600030721     C                   EVAL      VABNCL = *zeros
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800040122     C                             + ' ' + 'VABNCL'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** PKB
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiNum=*on
117600130610     C                   Z-ADD(H)  PiVal         VABPKB
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      VABPKB = *zeros
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABPKB'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** CAS
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118600110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
118700111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
118800111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
118900130703     C                   EVAL      VABVCA = 'EUR'
119000101018     C                   EVAL      wFlgCAS = '1'
119100101018     C                   ENDIF
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiNum=*on
119500030721     C                   Z-ADD     PiVal         VABCAS
119600030721     C                   ELSE
119700030721     C                   SETON                                        32
119800030721     C                   EVAL      VABCAS = *zeros
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000030721     C                             + ' ' + 'VABCAS'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** CCM
120400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
120500030721     C                   EVAL      PiStr=SkSplitCSV(i)
120600030721     C                   EXSR      CHKNUM
120700030721     C                   IF        PiInt=*on      AND
120800030721     C                             PiVal<=9999999 AND
120900030721     C                             PiVal>*zeros
121000030721     C                   Z-ADD     PiVal         VABCCM
121100030721     C                   ELSE
121200030721     C                   SETON                                        32
121300030721     C                   EVAL      VABCCM = *zeros
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500040510     C                             + ' ' + 'VABCCM'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** LNP
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiInt=*on    AND
122300030721     C                             PiVal<=999   AND
122400030721     C                             PiVal>*zeros
122500030721     C                   Z-ADD     PiVal         VABLNP
122600030721     C                   ELSE
122700040715     C                   SETON                                        32
122800030721     C                   EVAL      VABLNP = *zeros
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000040510     C                             + ' ' + 'VABLNP'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** NRS
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on AND
123800030721     C                             PiVal<=99
123900030721     C                   Z-ADD     PiVal         VABNRS
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124300030721     C                             + ' ' + 'VABNRS'
124400030721     C                   ENDIF
124500030721     C                   ENDIF
124600030721     C*** LNA
124700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124800030721     C                   EVAL      PiStr=SkSplitCSV(i)
124900030721     C                   EXSR      CHKNUM
125000030721     C                   IF        PiInt=*on    AND
125100030721     C                             PiVal<=999   AND
125200030721     C                             PiVal>*zeros
125300030721     C                   Z-ADD     PiVal         VABLNA
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      VABLNA = *zeros
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABLNA'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** CTR
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiInt=*on  AND
126600030721     C                             PiVal<=999
126700030721     C                   Z-ADD     PiVal         VABCTR
126800030721     C                   ELSE
126900030721     C                   SETON                                        32
127000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127100030721     C                             + ' ' + 'VABCTR'
127200030721     C                   ENDIF
127300030721     C                   ENDIF
127400030721     C*** IAS
127500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127600030721     C                   EVAL      PiStr=SkSplitCSV(i)
127700030721     C                   EXSR      CHKNUM
127800030721     C                   IF        PiNum=*on
127900030721     C                   Z-ADD     PiVal         VABIAS
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300030721     C                             + ' ' + 'VABIAS'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** VLB
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiNum=*on
129100130610     C                   Z-ADD(H)  PiVal         VABVLB
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129500030721     C                             + ' ' + 'VABVLB'
129600030721     C                   ENDIF
129700030721     C                   ENDIF
129800030721     C*** QFT
129900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
130000030721     C                   EVAL      PiStr=SkSplitCSV(i)
130100030721     C                   EXSR      CHKNUM
130200030721     C                   IF        PiNum=*on
130300030721     C                   Z-ADD     PiVal         VABQFT
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABQFT'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** NCD
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on      AND
131500030721     C                             PiVal<=9999999
131600030721     C                   Z-ADD     PiVal         VABNCD
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABNCD'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** NCA
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on      AND
132800030721     C                             PiVal<=9999999
132900030721     C                   Z-ADD     PiVal         VABNCA
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABNCA'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600030721     C*** ZNC
133700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiInt=*on AND
134100030721     C                             PiVal<=99
134200030721     C                   Z-ADD     PiVal         VABZNC
134300030721     C                   ELSE
134400030721     C                   SETON                                        32
134500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134600030721     C                             + ' ' + 'VABZNC'
134700030721     C                   ENDIF
134800030721     C                   ENDIF
134900030721     C*** DCR
135000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
135100030721     C                   EVAL      PiStr=SkSplitCSV(i)
135200030721     C                   EXSR      CHKNUM
135300030721     C                   IF        PiInt=*on       AND
135400030721     C                             PiVal<=99999999
135500030721     C                   Z-ADD     PiVal         VABDCR
135600030721     C                   ELSE
135700030721     C                   SETON                                        32
135800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135900030721     C                             + ' ' + 'VABDCR'
136000030721     C                   ENDIF
136100030721     C                   ENDIF
136200030721     C*** HCR
136300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136400030721     C                   EVAL      PiStr=SkSplitCSV(i)
136500030721     C                   EXSR      CHKNUM
136600030721     C                   IF        PiInt=*on   AND
136700030721     C                             PiVal<=9999
136800030721     C                   Z-ADD     PiVal         VABHCR
136900030721     C                   ELSE
137000030721     C                   SETON                                        32
137100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137200030721     C                             + ' ' + 'VABHCR'
137300030721     C                   ENDIF
137400030721     C                   ENDIF
137500030721     C*** VMD
137600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137700030721     C                   EVAL      PiStr=SkSplitCSV(i)
137800030721     C                   EXSR      CHKNUM
137900030721     C                   IF        PiNum=*on
138000030721     C                   Z-ADD     PiVal         VABVMD
138100030721     C                   ELSE
138200030721     C                   SETON                                        32
138300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138400030721     C                             + ' ' + 'VABVMD'
138500030721     C                   ENDIF
138600030721     C                   ENDIF
138700030721     C*** ANT
138800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138900030721     C                   EVAL      PiStr=SkSplitCSV(i)
139000030721     C                   EXSR      CHKNUM
139100030721     C                   IF        PiInt=*on        AND
139200030721     C                             PiVal<=999999999
139300030721     C                   Z-ADD     PiVal         VABANT
139400030721     C                   ELSE
139500030721     C                   SETON                                        32
139600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139700030721     C                             + ' ' + 'VABANT'
139800030721     C                   ENDIF
139900030721     C                   ENDIF
140000030721     C*** CMO
140100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
140200030721     C                   EVAL      PiStr=SkSplitCSV(i)
140300030721     C                   EXSR      CHKNUM
140400030721     C                   IF        PiInt=*on
140500030721     C                   Z-ADD     PiVal         Num5_0
140600030721     C                   MOVEL(p)  Num5_0        VABCMO
140700030721     C                   ELSE
140800030721     C                   SETON                                        32
140900030721     C                   EVAL      VABCMO = *zeros
141000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141100030721     C                             + ' ' + 'VABCMO'
141200030721     C                   ENDIF
141300030721     C                   ENDIF
141400040510     C*
141500040510     C* Reperisco i campi del FIVAT
141600040510     C*** VATNOT
141700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141900040510     C                   ENDIF
142000040510     C*** VATTRC
142100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
142200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
142300040510     C                   ENDIF
142400060331     C*** VATNOT_A
142500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142700060331     C                   ENDIF
142800060331     C*** VATNOT_B
142900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
143000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
143100060331     C                   ENDIF
143200060331     C*** VATNOT_E
143300070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143400060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143500060331     C                   ENDIF
143600110927     C*** VATNOT_I
143700131129     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
143800131129     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
143900110927     C                   ENDIF
144000070823     C*
144100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
144200120301
144300130805      *** W_VATNOT_E: deve essere numerico perché viena calcolato il check digit
144400130805      *** dopodiché è sia VABRMA che VABRMN ma solo gli ultimi 15 char a dx
144500130610     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
144600130610     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144700130805     C                   EVAL      PiStr=wVATNOT_E
144800130805     C                   EXSR      CHKNUM
144900130805     C                   IF        PiInt=*on
145000130805     C                   EVAL      VABRMA =
145100130805     C                              %subst(wVATNOT_E:%scan(' ':wVATNOT_E)-15:15)
145200130805     C                   EVAL      VABRMN = %dec(VABRMA:15:0)
145300130805     C                   ELSE
145400130805     C                   EVAL      wVATNOT_E = *blank
145500130805     C                   SETON                                        32
145600130805     C                   EVAL      vinmsg = %trimr(vinmsg)
145700130805     C                             + ' ' + 'VATNOT_E VABRMA VABRMN'
145800130805     C                   ENDIF
145900120411     C                   ENDIF
146000130610
146100130610      *** W_VABNOT: è sia VABNOT che VABNT2
146200130610     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
146300130610     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
146400130610     C                   EVAL      VABNOT =
146500130610     C                              %subst(wNOT:1:35)
146600130610     C                   EVAL      VABNT2 =
146700130610     C                              %subst(wNOT:36:35)
146800130610     C                   ENDIF
146900070925     C*
147000071029     C***  <===  -----------------------------------------
147100040510     C*
147200030715     C                   ENDIF
147300030715     C                   EVAL      i = i + 1
147400030715     C                   ENDDO
147500070925     C*
147600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147700120411     C*
147800130626     C* Forzatura tipo incasso 'BM' in caso di non indicazione specifica dal cliente
147900130703     C                   IF        wFlgCAS = '1'
148000140422     C                   IF        VABTIC = *blanks AND VABCCM <> 0506138
148100130626     C                   EVAL      VABTIC = 'BM'
148200130626     C                   ENDIF
148300130703     C                   ENDIF
148400100831     C*
148500131129     C* email destinatario
148600131129     C                   IF        %trim(wVATNOT_IJ) <> *blanks
148700131129     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
148800131129     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:35)
148900131129     C                   ENDIF
149000070925     C*
149100071029     C***  <===  -----------------------------------------
149200070925     C*
149300070925     C* Considerazioni finali su CBO/CAS
149400100318     C                   IF        wFlgCAS = '1'    OR
149500100318     C                             VABCAS  > *zeros
149600070925     C                   IF        VABCBO = '1'
149700070925     C                   EVAL      VABCBO = '4'
149800070925     C                   ENDIF
149900070925     C                   IF        VABCBO = '2'
150000070925     C                   EVAL      VABCBO = '6'
150100070925     C                   ENDIF
150200070925     C                   ENDIF
150300070925     C***  <===  ----------------------------
150400040510     C*
150500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
150600040510     C                   IF        VABNSP = *zeros
150700040510     C                   SETON                                        31
150800040510     C                   ENDIF
150900040715     C*
151000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
151100040715     C                   IF        VABLNP = *zeros
151200040715     C                   SETON                                        31
151300040715     C                   ENDIF
151400040510     C*
151500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
151600120411     C                   IF        VABRMN = *zeros
151700120411     C                   Z-ADD     VABNSP        VABRMN
151800120411     C                   ENDIF
151900120328     C* IN QUESTO CASO LO FORZO = 1
152000120411     C***                IF        VABRMN = *zeros
152100120411     C***                Z-ADD     1             VABRMN
152200120411     C***                ENDIF
152300040510     C*
152400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
152500040510     C                   IF        VABRMA = *blanks
152600040714     C                   MOVEL     VABRMN        VABRMA
152700040510     C                   ENDIF
152800040128     C*
152900040510     C* Se provincia nn valorizzata la reperisco
153000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
153100110912     C                   IF        VABPRD =  *blanks AND
153200110912     C                             VABNZD =  *blanks AND
153300110912     C                             VABCAD <> *blanks AND
153400110912     C                             VABLOD <> *blanks
153500040128     C                   CLEAR                   TISI95DS
153600040128     C                   EVAL      I95TCN = '3'
153700040128     C                   Z-ADD     datcor        I95DAT
153800040128     C                   EVAL      I95NAR = VABNZD
153900040128     C                   EVAL      I95CAP = VABCAD
154000040128     C                   EVAL      I95LOC = VABLOD
154100040128     C                   CALL      'TISI95R'
154200040128     C                   PARM                    TISI95DS
154300040128     C                   EVAL      VABPRD = O95PRV
154400040510     C                   ENDIF
154500020204     C*
154600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
154700020204     C                   EXSR      CHKIMPDIV
154800030715     C                   ENDIF
154900020204     C*
155000010607     C                   ENDSR
155100010607     C*----------------------------------------------------*
155200010601
155300020204
155400020204     C*----------------------------------------------------*
155500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
155600020204     C*----------------------------------------------------*
155700020204     C     CHKIMPDIV     BEGSR
155800020204     C*
155900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
156000020204     C                   Z-ADD     *zeros        wrkDec            9 9
156100020204     C*
156200020204     C* Come prima cosa effettuo considerazioni sulla divisa
156300020204     C                   IF        vabIAS > *zeros
156400020204     C                   IF        vabVAS <> 'EUR'
156500020204     C                   EVAL      vabVAS =  'ITL'
156600020204     C                   ENDIF
156700020204     C                   ENDIF
156800020204     C*
156900020204     C                   IF        vabCAS > *zeros
157000020204     C                   IF        vabVCA <> 'EUR'
157100020204     C                   EVAL      vabVCA =  'ITL'
157200020204     C                   ENDIF
157300020204     C                   ENDIF
157400020204     C*
157500020204     C                   IF        vabVMD > *zeros
157600020204     C                   IF        vabVAD <> 'EUR'
157700020204     C                   EVAL      vabVAD =  'ITL'
157800020204     C                   ENDIF
157900020204     C                   ENDIF
158000020204     C*
158100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
158200020204     C                   Z-ADD     vabIAS        wrkDec
158300020204     C                   IF        wrkDec > *zeros
158400020204     C                   IF        vabVAS = 'ITL'
158500020204     C                   EVAL      vabIAS = *zeros
158600020204     C                   ENDIF
158700020204     C                   ENDIF
158800020204     C*
158900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
159000020204     C                   Z-ADD     vabCAS        wrkDec
159100020204     C                   IF        wrkDec > *zeros
159200020204     C                   IF        vabVCA = 'ITL'
159300020204     C                   EVAL      vabCAS = *zeros
159400020204     C                   ENDIF
159500020204     C                   ENDIF
159600020204     C*
159700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
159800020204     C                   Z-ADD     vabVMD        wrkDec
159900020204     C                   IF        wrkDec > *zeros
160000020204     C                   IF        vabVAD = 'ITL'
160100020204     C                   EVAL      vabVMD = *zeros
160200020204     C                   ENDIF
160300020204     C                   ENDIF
160400020204     C*
160500020204     C                   ENDSR
160600020204     C***
160700010330
160800100318
160900100318
161000100318     C*----------------------------------------------------*
161100100318     C*  CONTROLLO NUMERICITA' CAMPI
161200100318     C*----------------------------------------------------*
161300100318     C     CHKNUM        BEGSR
161400100318     C*
161500100318     C                   IF        PiDecChr = *blanks
161600100318     C                   EVAL      PiDecChr = CharNUM
161700100318     C                   ENDIF
161800100318     C*
161900100318     C                   callp     UBISNUM_Check(PiStr
162000100318     C                                          :PiDecChr
162100100318     C                                          :PiVal
162200100318     C                                          :PiNum
162300100318     C                                          :PiInt)
162400100318     C*
162500100318     C                   ENDSR
162600100318     C***
162700010601
162800010601
162900010601
163000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
163100010601     C     repfil        BEGSR
163200010601     C*
163300010601     C                   if        invfil = *zeros and
163400010601     C                             depfil > *zeros and
163500010629     C                             (vinflg = *blanks or
163600010629     C                              vinflg = *zeros)
163700010601     C
163800010601     C                   eval      invfil = depfil
163900010601     C                   endif
164000010601     C*
164100010601     C                   if        depfil <> invfil and
164200010601     C                             invfil > *zeros
164300010601     C                   eval      flgMulti = '1'
164400010601     C                   if        vinflg = *blanks
164500010601     C                   add       1             cntNonEl
164600010601     C                   endif
164700010601     C                   endif
164800010601     C*
164900010601     C                   if        vinflg = '2'
165000010601     C                   eval      flgStato = '2'
165100010601     C                   endif
165200010601     C*
165300010601     C                   ENDSR
165400010601     C***
165500010601
165600010601
165700010601
165800010330
165900040119      /TITLE Invio dei dati al punto operativo.
166000040119     C     invio         BEGSR
166100040119     C*
166200040119     C* 1° invio FIVAT
166300040119     C                   reset                   dscmz
166400040119     C                   move      vlrpoi        cmzdst
166500040119     C                   eval      cmzfld = 'FIVATWWR'
166600040119     C                   eval      cmzmbd = vlrhdl
166700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166800040119     C***                if        prmfir = *blanks
166900040119     C                   eval      cmzfla = 'FIVAT00F'
167000040119     C                   eval      cmzmba = 'FIVAT00F'
167100040119     C***                else
167200040119     C***                eval      cmzfla = prmfir
167300040119     C***                eval      cmzmba = prmfir
167400040119     C***                endif
167500040119     C                   eval      cmznrr = *zeros
167600040119     C                   move      §ctrokvt      cmznrr
167700040119     C                   eval      cmzlba = vlrfl1
167800040119     C                   call(e)   'TIS711C'
167900040119     C                   parm                    dscmz
168000040119     C                   parm      *blanks       esito
168100040119     C                   if        %error
168200040119     C                             or cmzerr = '1'
168300040119     C                             or esito  = '1'
168400040119     C                   eval      wrkesito = '3'
168500040119     C                   else
168600040119     C*
168700040119     C* 2° invio FIVAB
168800040119     C                   reset                   dscmz
168900040119     C                   move      vlrpoi        cmzdst
169000040119     C                   eval      cmzfld = vlrfou
169100040119     C                   eval      cmzmbd = vlrhdl
169200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169300040119     C***                if        prmfir = *blanks
169400040119     C                   eval      cmzfla = 'FIVAB00F'
169500040119     C                   eval      cmzmba = 'FIVAB00F'
169600040119     C***                else
169700040119     C***                eval      cmzfla = prmfir
169800040119     C***                eval      cmzmba = prmfir
169900040119     C***                endif
170000040119     C                   eval      cmznrr = *zeros
170100040119     C                   move      §ctrokvb      cmznrr
170200040119     C                   eval      cmzlba = vlrfl1
170300040119     C                   call(e)   'TIS711C'
170400040119     C                   parm                    dscmz
170500040119     C                   parm      *blanks       esito
170600040119     C                   if        %error
170700040119     C                             or cmzerr = '1'
170800040119     C                             or esito  = '1'
170900040119     C                   eval      wrkesito = '3'
171000040119     C                   endif
171100040119     C                   endif
171200040119     C*
171300040119     C                   ENDSR
171400040119     C***
171500010601
171600010601
171700010601
171800010601
171900010601      /TITLE Invio dei dati al punto operativo.
172000010601     C     opeini        BEGSR
172100010601     C*
172200010601     C* Inizializzo flag e contatori operativi
172300010601     C                   movel     '0'           flgGiro           1
172400010601     C                   movel     '0'           flgMulti          1
172500010601     C                   movel     '1'           flgStato          1
172600010615     C                   movel     '0'           flgOk             1
172700010601     C                   z-add     *zeros        cntNonEl         10 0
172800010601     C                   z-add     *zeros        depfil            3 0
172900010601     C                   z-add     *zeros        invfil            3 0
173000010601     C*
173100010601     C                   ENDSR
173200010601     C***
173300070326
173400070326
173500070326
173600070326
173700070326     C     *pssr         BEGSR
173800070329     C*
173900070329     C                   if        %open(tivin00r)
174000070329     C                   close     tivin00r
174100070329     C                   endif
174200070329     C                   if        %open(fivabwwr)
174300070329     C                   close     fivabwwr
174400070329     C                   endif
174500070329     C                   if        %open(fivatwwr)
174600070329     C                   close     fivatwwr
174700070329     C                   endif
174800070326     C*
174900070326     C* Effettuo la chiamata al CLLE preposto
175000070326     C                   call(e)   'TITVVTC'
175100070326     C                   parm                    parccm
175200070326     C                   parm                    parmbr
175300070326     C                   parm      '2'           paropz
175400070326     C*
175500070326     C                   eval      wrkesito = '2'
175600070404     C*
175700070404     C                   seton                                        LR
175800070326     C*
175900070326     C                   ENDSR     '*CANCL'
176000070326     C***
176100100318
176200100318
176300100318
176400100318     C     normCSV       BEGSR
176500100318     C*
176600100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
176700100318     C* separatori campi
176800100318     C                   if        CharTXT <> *blanks
176900100318     C                   z-add     *zeros        wLenVINDTA        4 0
177000100318     C                   z-add     *zeros        wFlgVINDTA        1 0
177100100318     C                   setoff                                       80
177200100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
177300100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
177400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
177500100318     C                   if        not *in80
177600100318     C                   seton                                        80
177700100318     C                   else
177800100318     C                   setoff                                       80
177900100318     C                   endif
178000100318     C                   endif
178100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
178200100318     C                             and *in80 = *on
178300100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
178400100318     C                   endif
178500100318     C                   enddo
178600100318     C                   endif
178700100318     C*
178800100318     C                   ENDSR
178900100318     C***
179000070326
179100100326
179200100326
179300100326     C*----------------------------------------------------*
179400100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
179500100326     C*----------------------------------------------------*
179600100326     C     REPNSP        BEGSR
179700100326     C*
179800100326     C* NSP => Stacco un numeratore da AZNUM
179900100326     C                   clear                   TRUL33DS
180000100326     C                   eval      I33OPE = *zeros
180100100326     C                   eval      I33CNU = 302
180200100326     C                   eval      I33NUM = 1
180300100326     C                   movel     TRUL33DS      KPJBU
180400100326     C                   call      'TRUL33R'
180500100326     C                   parm                    KPJBA
180600100326     C                   movel     KPJBU         TRUL33DS
180700100326     C                   if        O33ERR = *zeros
180800100326     C                   z-add     O33NRF        VABNSP
180900100326     C                   z-add     O33NRF        VATNSP
181000100326     C                   else
181100100326     C                   SETON                                        31
181200100326     C                   EVAL      vinmsg = %trimr(vinmsg)
181300100326     C                             + ' ' + 'VABNSP VATNSP'
181400100326     C                   endif
181500100326     C*
181600100326     C                   ENDSR
181700010330
181800100326
181900010330
182000000613     C     *inzsr        BEGSR
182100990910     C*
182200990910     C     *entry        plist
182300990920     C                   parm                    tivlrds
182400990921     C                   parm      wrkesito      esito
182500000724     C                   parm                    prmlit
182600000710     C                   parm                    prmfir
182700010330     C*
182800010330     C* CALCOLA LA DATA CORRENTE
182900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
183000100318     C                   eval      datcor = %dec(%date() : *ISO)
183100000613     C*
183200000613     C                   ENDSR
183300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
183400130610W_VATNOT_E;VATNOT_A;VABRSD;VABIND;VABLOD;VABCAD;VATNOT_B;VABCAS;VABPKB;VABVLB;VABNCL;
183500131129W_VABNOT;VABTIC;W_VATNOT_I
183600130528
183700111116
183800100318
