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
021500010601     C*
021600070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021700130605     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
021800130605     C* evitare che vengano elisi i blank
021900130610     C                   eval      vindta  = ' ' + %trim(vindta)
022000070925     C*
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
041800120320     C*
041900120410     C* i rcd di FIVAT con tipo E sono 1 in quanto è una spedizione monocollo
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
046000040119     C*
046100040119     C                   ENDSR
046200040119
046300040119
046400040119
046500040119     C*----------------------------------------------------*
046600040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046700040119     C*----------------------------------------------------*
046800040119     C     PREVAT        BEGSR
046900040119     C*
047000040119     C* Compongo il nome del membro da dare al FIVATWWR
047100040119     C                   eval      parmbr = vlrhdl
047200040119     C                   movel     'M'           parmbr
047300040510     C                   eval      parccm = vlrksc
047400040119     C                   eval      paropz = '1'
047500040119     C* Effettuo la chiamata al CLLE preposto
047600040119     C                   call(e)   'TITVVTC'
047700040119     C                   parm                    parccm
047800040119     C                   parm                    parmbr
047900040119     C                   parm                    paropz
048000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048100040119     C                   if        %error
048200040119     C                   movel     '1'           chkcall
048300040119     C                   else
048400040119     C                   movel     '0'           chkcall
048500040119     C                   endif
048600040119     C*
048700040119     C                   ENDSR
048800010601
048900010601
049000010601
049100010330     C*----------------------------------------------------*
049200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049300010330     C*----------------------------------------------------*
049400040119     C     INZVAR        BEGSR
049500080527     C*
049600080527     C                   setoff                                       65
049700010330     C*
049800030822     C* Inizializzo variabili di wrk
049900020204     C                   Z-ADD     *zeros        Num5_0            5 0
050000101018     C                   MOVEL     *blanks       wFlgCAS
050100060331     C                   MOVEL     *blanks       wVATNOT_A
050200060331     C                   MOVEL     *blanks       wVATNOT_B
050300060331     C                   MOVEL     *blanks       wVATNOT_E
050400110927     C                   MOVEL     *blanks       wVATNOT_I
050500120411     C                   MOVEL     *blanks       wNOT
050600120411     C                   MOVEL     *blanks       Data8A
050700030822     C*
050800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050900030822     C                   CLEAR                   SkSplitCSV
051000030822     C                   EVAL      i = 1
051100030822     C                   EVAL      posDa = *zeros
051200030822     C                   EVAL      posA  = *zeros
051300080527     C*
051400080527     C                   CLEAR                   FIVABDS
051500080527     C                   CLEAR                   FIVAT000
051600030822     C*
051700030822     C* Reimposto i valori di default
051800030822     C                   EXSR      DEFCAM
051900030822     C*
052000010330     C                   ENDSR
052100010330     C*----------------------------------------------------*
052200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052300010330     C*----------------------------------------------------*
052400010330     C     DEFCAM        BEGSR
052500030822     C*
052600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052700030715     C* e delimitatore testo.
052800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
053100100318     C*
053200100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
053300100318     C                   EVAL      CharSOS = CharNUM
053400030822     C*
053500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053600100720     C                   SETOFF                                       505152
053700100720     C                   SETOFF                                       53
053800100413     C                   SELECT
053900100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
054000100720     C                   SETON                                        50
054100100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
054200100413     C                   SETON                                        51
054300100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
054400100720     C                   SETON                                        52
054500100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
054600100720     C                   SETON                                        53
054700100413     C                   ENDSL
054800030822     C*
054900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055000030822     C                   EVAL      posDaDft = 1
055100030822     C                   EVAL      posADft  = 0
055200030822     C                   EVAL      wGiroDft = 0
055300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055400030822     C                             posDaDft > 0
055500030822     C*
055600030822     C* Gestisco il 1° giro
055700030822     C                   IF        wGiroDft = 0
055800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056000030822     C* Incremento il contatore dei "giri"
056100030822     C                   EVAL      wGiroDft = 1
056200030822     C                   ELSE
056300030822     C                   EVAL      posDaDft = posADft
056400030822     C                   ENDIF
056500030822     C* Eseguo lo scan x trovare la fine del campo corrente
056600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056700030822     C*
056800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057000030822     C                   IF        posDaDft > 0 AND
057100030822     C                             posADft  > 0
057200030822     C* NCL
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'NCL'
057700030822     C                   EVAL      PiStr=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   EXSR      CHKNUM
058100030822     C                   IF        PiInt=*on
058200030822     C                   Z-ADD     PiVal         VABNCL
058300030822     C                   ENDIF
058400030822     C                   ENDIF
058500030822     C* CCM
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'CCM'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiInt=*on
059500030822     C                   Z-ADD     PiVal         VABCCM
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* LNP
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'LNP'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiInt=*on
060800030822     C                   Z-ADD     PiVal         VABLNP
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* NRS
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'NRS'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiInt=*on
062100030822     C                   Z-ADD     PiVal         VABNRS
062200030822     C                   ENDIF
062300030822     C                   ENDIF
062400030822     C* CTR
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'CTR'
062900030822     C                   EVAL      PiStr=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   EXSR      CHKNUM
063300030822     C                   IF        PiInt=*on
063400030822     C                   Z-ADD     PiVal         VABCTR
063500030822     C                   ENDIF
063600030822     C                   ENDIF
063700030822     C* PKB
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'PKB'
064200030822     C                   EVAL      PiStr=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   EXSR      CHKNUM
064600030822     C                   IF        PiNum=*on
064700130610     C                   Z-ADD     PiVal         VABPKB
064800030822     C                   ENDIF
064900030822     C                   ENDIF
065000030822     C* VLB
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'VLB'
065500030822     C                   EVAL      PiStr=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   EXSR      CHKNUM
065900030822     C                   IF        PiNum=*on
066000130610     C                   Z-ADD     PiVal         VABVLB
066100030822     C                   ENDIF
066200030822     C                   ENDIF
066300030822     C* QFT
066400030822     C                   IF        %subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):1:3)
066700030822     C                             = 'QFT'
066800030822     C                   EVAL      PiStr=%trim(%subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100030822     C                   EXSR      CHKNUM
067200030822     C                   IF        PiNum=*on
067300030822     C                   Z-ADD     PiVal         VABQFT
067400030822     C                   ENDIF
067500030822     C                   ENDIF
067600030822     C* CBO
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'CBO'
068100030822     C                   EVAL      VABCBO=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500030822     C* TSP
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'TSP'
069000030822     C                   EVAL      VABTSP=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400030822     C* VAS
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'VAS'
069900030822     C                   EVAL      VABVAS=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* VCA
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'VCA'
070800030822     C                   EVAL      VABVCA=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200070925     C* TIC
071300070925     C                   IF        %subst(
071400070925     C                             %subst(vlrppt:posDaDft+1:
071500070925     C                             posADft-posDaDft-1):1:3)
071600070925     C                             = 'TIC'
071700070925     C                   EVAL      VABTIC=%trim(%subst(
071800070925     C                             %subst(vlrppt:posDaDft+1:
071900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000070925     C                   ENDIF
072100030822     C* GCA
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'GCA'
072600030822     C                   EVAL      VABGCA=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* CTM
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'CTM'
073500030822     C                   EVAL      VABCTM=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* FFD
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'FFD'
074400030822     C                   EVAL      VABFFD=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* VAD
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'VAD'
075300030822     C                   EVAL      VABVAD=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* GMA
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'GMA'
076200030822     C                   EVAL      VABGMA=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* GGA
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'GGA'
077100030822     C                   EVAL      VABGGA=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500030822     C* GVA
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'GVA'
078000030822     C                   EVAL      VABGVA=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400030822     C* TC1
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'TC1'
078900030822     C                   EVAL      VABTC1=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300030822     C* TC2
079400030822     C                   IF        %subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):1:3)
079700030822     C                             = 'TC2'
079800030822     C                   EVAL      VABTC2=%trim(%subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100030822     C                   ENDIF
080200040714     C* VATTRC
080300040714     C                   IF        %subst(
080400040714     C                             %subst(vlrppt:posDaDft+1:
080500040714     C                             posADft-posDaDft-1):1:3)
080600040714     C                             = 'TRC'
080700040714     C                   EVAL      VATTRC=%trim(%subst(
080800040714     C                             %subst(vlrppt:posDaDft+1:
080900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081000040714     C                   ENDIF
081100030822     C* ...
081200030822     C                   ENDIF
081300030822     C                   ENDDO
081400020204     C*
081500010330     C                   ENDSR
081600010607     C*----------------------------------------------------*
081700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
081800010607     C*----------------------------------------------------*
081900010607     C     IMPVAB        BEGSR
082000010607     C*
082100010607     C                   SETOFF                                       3132
082200110315 xxx C***                SETON                                        35
082300030515     C*
082400010607     C                   MOVEL     datcor        VABAAS
082500010607     C                   MOVE      datcor        VABMGS
082600010607     C*
082700030715     C**********
082800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082900030715     C**********
083000030715     C                   IF        wGiro = *zeros
083100040113     C*
083200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
083300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
083400080429     C                             %trim(intcol(3))+%trim(intcol(4))+
083500080429     C                             %trim(intcol(5))<>*blanks
083600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083700080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
083800080429     C                                      %trim(intcol(5))
083900070823     C     ';':CharCSV   XLATE     vindta        vindta
084000040510     C                   ENDIF
084100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084200031201     C     minu:maiu     XLATE     vindta        vindta
084300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084400030715     C                   DOW       posDa <= %len(%trim(vindta))
084500030715     C*
084600030715     C* Gestisco il 1° campo
084700030715     C                   IF        i = 1
084800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084900030715     C                   EVAL      posDa = 1
085000030715     C* Eseguo lo scan x trovare la fine del primo campo
085100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085200030715     C* A questo punto "estraggo" il campo corrente
085300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085400030715     C                                                    (posA-posDa))
085500030715     C* X i campi successivi al 1°
085600030715     C                   ELSE
085700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085800030715     C                   EVAL      posDa = posA + 1
085900030715     C* Eseguo lo scan x trovare la fine del campo corrente
086000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086100030715     C* Gestisco l'ultimo campo
086200030715     C                   IF        posA = *zeros
086300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086400030715     C* A questo punto "estraggo" il campo corrente
086500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086600030715     C                                                    (posA-posDa))
086700030715     C                   LEAVE
086800030715     C                   ELSE
086900030715     C* A questo punto "estraggo" il campo corrente
087000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087100030715     C                                                    (posA-posDa))
087200030715     C                   ENDIF
087300030715     C                   ENDIF
087400030715     C* Incremento il contatore d campo
087500030715     C                   EVAL      i = i +1
087600030715     C                   ENDDO
087700030715     C*
087800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087900030715     C* delimitazione testo
088000030715     C                   EVAL      i = 1
088100030715     C                   DOW       i < %elem(SkSplitFLD)
088200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
088300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
088400030715     C                   EVAL      i = i + 1
088500030715     C                   ENDDO
088600030715     C                   EVAL      wGiro = 1
088700030715     C                   ELSE
088800030715     C                   EVAL      wGiro = 2
088900031204     C**********
089000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
089100031204     C* d separatore campo
089200031204     C**********
089300031204     C                   EVAL      vindta = %trim(vindta)
089400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089500031204     C                   EVAL      vindta = %subst(vindta:2)
089600031204     C                   ENDDO
089700031204     C*
089800031204     C                   Z-ADD     *zeros        lunghInput        4 0
089900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
090100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
090200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090300110912     C                   ENDDO
090400030715     C**********
090500030715     C* Effettuo lo split del campo dati d input
090600030715     C**********
090700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090800030715     C                   DOW       posDa <= %len(%trim(vindta))
090900030715     C*
091000030715     C* Gestisco il 1° campo
091100030715     C                   IF        i = 1
091200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091300030715     C                   EVAL      posDa = 1
091400030715     C* Eseguo lo scan x trovare la fine del primo campo
091500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091600030715     C* A questo punto "estraggo" il campo corrente
091700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091800030715     C                                                    (posA-posDa))
091900030715     C* X i campi successivi al 1°
092000030715     C                   ELSE
092100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092200030715     C                   EVAL      posDa = posA + 1
092300031204     C* Verifico che nn vi sia il campo nn valorizzato
092400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092500031204     C* Se campo nn valorizzato skippo al prossimo
092600031204     C                   EVAL      SkSplitCSV(i) = *blanks
092700031204     C                   EVAL      posA  = posA + 1
092800031204     C                   ELSE
092900030715     C* Eseguo lo scan x trovare la fine del campo corrente
093000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093100030715     C* Gestisco l'ultimo campo
093200030715     C                   IF        posA = *zeros
093300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093400030715     C* A questo punto "estraggo" il campo corrente
093500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093600030715     C                                                    (posA-posDa))
093700030715     C                   LEAVE
093800030715     C                   ELSE
093900030715     C* A questo punto "estraggo" il campo corrente
094000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094100030715     C                                                    (posA-posDa))
094200030715     C                   ENDIF
094300031204     C                   ENDIF
094400030715     C                   ENDIF
094500030715     C* Incremento il contatore d campo
094600030715     C                   EVAL      i = i +1
094700030715     C                   ENDDO
094800030715     C*
094900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095000030715     C* delimitazione testo
095100030715     C                   EVAL      i = 1
095200030715     C                   DOW       i < %elem(SkSplitCSV)
095300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095500030715     C                   EVAL      i = i + 1
095600030715     C                   ENDDO
095700100720     C*
095800100720     C* Stacco un progressivo x numero spedizione
095900130610 xxx C                   EXSR      repNSP
096000100326     C*
096100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096200030715     C                   EVAL      i = 1
096300030715     C                   DOW       i < %elem(SkSplitFLD)
096400030715     C*
096500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096700030715     C                             SkSplitCSV(i) <> *blanks
096800030721     C*** ATB
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097000030721     C                   EVAL      VABATB = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030715     C*** RSD
097300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
097400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097700030715     C* ==
097800130528     C                   ENDIF
097900030721     C*** RD2
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
098100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030715     C*** IND
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
098500030715     C                   EVAL      VABIND = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** LOD
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** PRD
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030721     C*** NZD
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030715     C*** RMA
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
100100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030715     C*** NOT
100400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100600030715     C                   ENDIF
100700030715     C*** NT2
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030715     C*** VCA
101200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030715     C*** VAS
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030715     C*** FFD
102000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
102100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
102200030715     C                   ENDIF
102300030721     C*** GC1
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** GC2
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** TSP
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
103400030721     C                   ENDIF
103500040510     C*** CBO
103600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103800040510     C                   ENDIF
103900030721     C*** NAS
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
104100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** TIC
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
104500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** GCA
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** XCO
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** CTM
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** TCR
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
106100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** CTS
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
106500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** FTM
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** VAD
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** GMA
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** GGA
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
108100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** GVA
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
108500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** TC1
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** TC2
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** SCL
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** RMO
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
110100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** NMO
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
110500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*
110800030721     C* Reperisco quindi i campi numerici...
110900040510     C*** NSP
111000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
111100040510     C                   EVAL      PiStr=SkSplitCSV(i)
111200040510     C                   EXSR      CHKNUM
111300040510     C                   IF        PiInt=*on
111400040510     C                   Z-ADD     PiVal         VABNSP
111500040510     C                   ELSE
111600040510     C                   SETON                                        31
111700040510     C                   EVAL      VABNSP = *zeros
111800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111900040510     C                             + ' ' + 'VABNSP'
112000040510     C                   ENDIF
112100040510     C                   ENDIF
112200071106     C*** RMN
112300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
112400071106     C                   EVAL      PiStr=SkSplitCSV(i)
112500071106     C                   EXSR      CHKNUM
112600071106     C                   IF        PiInt=*on
112700071106     C                   Z-ADD     PiVal         VABRMN
112800071106     C                   ELSE
112900071106     C                   SETON                                        32
113000071106     C                   EVAL      VABRMN = *zeros
113100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
113200071106     C                             + ' ' + 'VABRMN'
113300071106     C                   ENDIF
113400071106     C                   ENDIF
113500030721     C*** CAD
113600130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113700130610     C                   EVAL      PiStr=SkSplitCSV(i)
113800130610     C                   EXSR      CHKNUM
113900130610     C                   IF        PiInt=*on
114000130610     C                   Z-ADD     PiVal         Num5_0
114100130610     C                   MOVEL(p)  Num5_0        VABCAD
114200130610     C                   ELSE
114300130610     C                   SETON                                        32
114400130610     C                   EVAL      VABCAD = *zeros
114500130610     C                   EVAL      vinmsg = %trimr(vinmsg)
114600130610     C                             + ' ' + 'VABCAD'
114700130610     C                   ENDIF
114800130605     C                   ENDIF
114900030721     C*** NCL
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
115100030721     C                   EVAL      PiStr=SkSplitCSV(i)
115200030721     C                   EXSR      CHKNUM
115300030721     C                   IF        PiInt=*on
115400030721     C                   Z-ADD     PiVal         VABNCL
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      VABNCL = *zeros
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900040122     C                             + ' ' + 'VABNCL'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200030721     C*** PKB
116300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
116400030721     C                   EVAL      PiStr=SkSplitCSV(i)
116500030721     C                   EXSR      CHKNUM
116600030721     C                   IF        PiNum=*on
116700130610     C                   Z-ADD(H)  PiVal         VABPKB
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      VABPKB = *zeros
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200030721     C                             + ' ' + 'VABPKB'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** CAS
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117700110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
117800111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
117900111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
118000120227 xxx C                   EVAL      VABvca = 'EUR'
118100101018     C                   EVAL      wFlgCAS = '1'
118200101018     C                   ENDIF
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiNum=*on
118600030721     C                   Z-ADD     PiVal         VABCAS
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      VABCAS = *zeros
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABCAS'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** CCM
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiInt=*on      AND
119900030721     C                             PiVal<=9999999 AND
120000030721     C                             PiVal>*zeros
120100030721     C                   Z-ADD     PiVal         VABCCM
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      VABCCM = *zeros
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600040510     C                             + ' ' + 'VABCCM'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** LNP
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiInt=*on    AND
121400030721     C                             PiVal<=999   AND
121500030721     C                             PiVal>*zeros
121600030721     C                   Z-ADD     PiVal         VABLNP
121700030721     C                   ELSE
121800040715     C                   SETON                                        32
121900030721     C                   EVAL      VABLNP = *zeros
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100040510     C                             + ' ' + 'VABLNP'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** NRS
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on AND
122900030721     C                             PiVal<=99
123000030721     C                   Z-ADD     PiVal         VABNRS
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123400030721     C                             + ' ' + 'VABNRS'
123500030721     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** LNA
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on    AND
124200030721     C                             PiVal<=999   AND
124300030721     C                             PiVal>*zeros
124400030721     C                   Z-ADD     PiVal         VABLNA
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      VABLNA = *zeros
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABLNA'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** CTR
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiInt=*on  AND
125700030721     C                             PiVal<=999
125800030721     C                   Z-ADD     PiVal         VABCTR
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126200030721     C                             + ' ' + 'VABCTR'
126300030721     C                   ENDIF
126400030721     C                   ENDIF
126500030721     C*** IAS
126600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
126700030721     C                   EVAL      PiStr=SkSplitCSV(i)
126800030721     C                   EXSR      CHKNUM
126900030721     C                   IF        PiNum=*on
127000030721     C                   Z-ADD     PiVal         VABIAS
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABIAS'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700030721     C*** VLB
127800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiNum=*on
128200130610     C                   Z-ADD(H)  PiVal         VABVLB
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABVLB'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** QFT
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiNum=*on
129400030721     C                   Z-ADD     PiVal         VABQFT
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABQFT'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** NCD
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on      AND
130600030721     C                             PiVal<=9999999
130700030721     C                   Z-ADD     PiVal         VABNCD
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABNCD'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** NCA
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on      AND
131900030721     C                             PiVal<=9999999
132000030721     C                   Z-ADD     PiVal         VABNCA
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABNCA'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** ZNC
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiInt=*on AND
133200030721     C                             PiVal<=99
133300030721     C                   Z-ADD     PiVal         VABZNC
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABZNC'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** DCR
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiInt=*on       AND
134500030721     C                             PiVal<=99999999
134600030721     C                   Z-ADD     PiVal         VABDCR
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABDCR'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** HCR
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiInt=*on   AND
135800030721     C                             PiVal<=9999
135900030721     C                   Z-ADD     PiVal         VABHCR
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABHCR'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600030721     C*** VMD
136700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
136800030721     C                   EVAL      PiStr=SkSplitCSV(i)
136900030721     C                   EXSR      CHKNUM
137000030721     C                   IF        PiNum=*on
137100030721     C                   Z-ADD     PiVal         VABVMD
137200030721     C                   ELSE
137300030721     C                   SETON                                        32
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABVMD'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800030721     C*** ANT
137900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138000030721     C                   EVAL      PiStr=SkSplitCSV(i)
138100030721     C                   EXSR      CHKNUM
138200030721     C                   IF        PiInt=*on        AND
138300030721     C                             PiVal<=999999999
138400030721     C                   Z-ADD     PiVal         VABANT
138500030721     C                   ELSE
138600030721     C                   SETON                                        32
138700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138800030721     C                             + ' ' + 'VABANT'
138900030721     C                   ENDIF
139000030721     C                   ENDIF
139100030721     C*** CMO
139200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139300030721     C                   EVAL      PiStr=SkSplitCSV(i)
139400030721     C                   EXSR      CHKNUM
139500030721     C                   IF        PiInt=*on
139600030721     C                   Z-ADD     PiVal         Num5_0
139700030721     C                   MOVEL(p)  Num5_0        VABCMO
139800030721     C                   ELSE
139900030721     C                   SETON                                        32
140000030721     C                   EVAL      VABCMO = *zeros
140100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140200030721     C                             + ' ' + 'VABCMO'
140300030721     C                   ENDIF
140400030721     C                   ENDIF
140500040510     C*
140600040510     C* Reperisco i campi del FIVAT
140700040510     C*** VATNOT
140800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
140900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141000040510     C                   ENDIF
141100040510     C*** VATTRC
141200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141400040510     C                   ENDIF
141500060331     C*** VATNOT_A
141600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
141700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
141800060331     C                   ENDIF
141900060331     C*** VATNOT_B
142000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142200060331     C                   ENDIF
142300060331     C*** VATNOT_E
142400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142600060331     C                   ENDIF
142700110927     C*** VATNOT_I
142800110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
142900110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
143000110927     C                   ENDIF
143100070823     C*
143200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
143300120301
143400130610      *** W_VATNOT_E: è sia VABRMA che VABRMN che VATNOT_E. ma solo gli ultimi 15 char a dx
143500130610     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
143600130610     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143700130528     C                   EVAL      VABRMA =
143800130610     C                              %subst(wVATNOT_E:%scan(' ':wVATNOT_E)-15:15)
143900130610     C                   EVAL      VABRMN = %dec(VABRMA:15:0)
144000120411     C                   ENDIF
144100130610
144200130610      *** W_VABNOT: è sia VABNOT che VABNT2
144300130610     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
144400130610     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
144500130610     C                   EVAL      VABNOT =
144600130610     C                              %subst(wNOT:1:35)
144700130610     C                   EVAL      VABNT2 =
144800130610     C                              %subst(wNOT:36:35)
144900130610     C                   ENDIF
145000070925     C*
145100071029     C***  <===  -----------------------------------------
145200040510     C*
145300030715     C                   ENDIF
145400030715     C                   EVAL      i = i + 1
145500030715     C                   ENDDO
145600070925     C*
145700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145800120411     C*
145900100831     C*
146000070925     C*
146100071029     C***  <===  -----------------------------------------
146200070925     C*
146300070925     C* Considerazioni finali su CBO/CAS
146400100318     C                   IF        wFlgCAS = '1'    OR
146500100318     C                             VABCAS  > *zeros
146600070925     C                   IF        VABCBO = '1'
146700070925     C                   EVAL      VABCBO = '4'
146800070925     C                   ENDIF
146900070925     C                   IF        VABCBO = '2'
147000070925     C                   EVAL      VABCBO = '6'
147100070925     C                   ENDIF
147200070925     C                   ENDIF
147300070925     C***  <===  ----------------------------
147400040510     C*
147500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
147600040510     C                   IF        VABNSP = *zeros
147700040510     C                   SETON                                        31
147800040510     C                   ENDIF
147900040715     C*
148000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
148100040715     C                   IF        VABLNP = *zeros
148200040715     C                   SETON                                        31
148300040715     C                   ENDIF
148400040510     C*
148500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
148600120411     C                   IF        VABRMN = *zeros
148700120411     C                   Z-ADD     VABNSP        VABRMN
148800120411     C                   ENDIF
148900120328     C* IN QUESTO CASO LO FORZO = 1
149000120411     C***                IF        VABRMN = *zeros
149100120411     C***                Z-ADD     1             VABRMN
149200120411     C***                ENDIF
149300040510     C*
149400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149500040510     C                   IF        VABRMA = *blanks
149600040714     C                   MOVEL     VABRMN        VABRMA
149700040510     C                   ENDIF
149800040128     C*
149900040510     C* Se provincia nn valorizzata la reperisco
150000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150100110912     C                   IF        VABPRD =  *blanks AND
150200110912     C                             VABNZD =  *blanks AND
150300110912     C                             VABCAD <> *blanks AND
150400110912     C                             VABLOD <> *blanks
150500040128     C                   CLEAR                   TISI95DS
150600040128     C                   EVAL      I95TCN = '3'
150700040128     C                   Z-ADD     datcor        I95DAT
150800040128     C                   EVAL      I95NAR = VABNZD
150900040128     C                   EVAL      I95CAP = VABCAD
151000040128     C                   EVAL      I95LOC = VABLOD
151100040128     C                   CALL      'TISI95R'
151200040128     C                   PARM                    TISI95DS
151300040128     C                   EVAL      VABPRD = O95PRV
151400040510     C                   ENDIF
151500020204     C*
151600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151700020204     C                   EXSR      CHKIMPDIV
151800030715     C                   ENDIF
151900020204     C*
152000010607     C                   ENDSR
152100010607     C*----------------------------------------------------*
152200010601
152300020204
152400020204     C*----------------------------------------------------*
152500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152600020204     C*----------------------------------------------------*
152700020204     C     CHKIMPDIV     BEGSR
152800020204     C*
152900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
153000020204     C                   Z-ADD     *zeros        wrkDec            9 9
153100020204     C*
153200020204     C* Come prima cosa effettuo considerazioni sulla divisa
153300020204     C                   IF        vabIAS > *zeros
153400020204     C                   IF        vabVAS <> 'EUR'
153500020204     C                   EVAL      vabVAS =  'ITL'
153600020204     C                   ENDIF
153700020204     C                   ENDIF
153800020204     C*
153900020204     C                   IF        vabCAS > *zeros
154000020204     C                   IF        vabVCA <> 'EUR'
154100020204     C                   EVAL      vabVCA =  'ITL'
154200020204     C                   ENDIF
154300020204     C                   ENDIF
154400020204     C*
154500020204     C                   IF        vabVMD > *zeros
154600020204     C                   IF        vabVAD <> 'EUR'
154700020204     C                   EVAL      vabVAD =  'ITL'
154800020204     C                   ENDIF
154900020204     C                   ENDIF
155000020204     C*
155100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155200020204     C                   Z-ADD     vabIAS        wrkDec
155300020204     C                   IF        wrkDec > *zeros
155400020204     C                   IF        vabVAS = 'ITL'
155500020204     C                   EVAL      vabIAS = *zeros
155600020204     C                   ENDIF
155700020204     C                   ENDIF
155800020204     C*
155900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156000020204     C                   Z-ADD     vabCAS        wrkDec
156100020204     C                   IF        wrkDec > *zeros
156200020204     C                   IF        vabVCA = 'ITL'
156300020204     C                   EVAL      vabCAS = *zeros
156400020204     C                   ENDIF
156500020204     C                   ENDIF
156600020204     C*
156700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156800020204     C                   Z-ADD     vabVMD        wrkDec
156900020204     C                   IF        wrkDec > *zeros
157000020204     C                   IF        vabVAD = 'ITL'
157100020204     C                   EVAL      vabVMD = *zeros
157200020204     C                   ENDIF
157300020204     C                   ENDIF
157400020204     C*
157500020204     C                   ENDSR
157600020204     C***
157700010330
157800100318
157900100318
158000100318     C*----------------------------------------------------*
158100100318     C*  CONTROLLO NUMERICITA' CAMPI
158200100318     C*----------------------------------------------------*
158300100318     C     CHKNUM        BEGSR
158400100318     C*
158500100318     C                   IF        PiDecChr = *blanks
158600100318     C                   EVAL      PiDecChr = CharNUM
158700100318     C                   ENDIF
158800100318     C*
158900100318     C                   callp     UBISNUM_Check(PiStr
159000100318     C                                          :PiDecChr
159100100318     C                                          :PiVal
159200100318     C                                          :PiNum
159300100318     C                                          :PiInt)
159400100318     C*
159500100318     C                   ENDSR
159600100318     C***
159700010601
159800010601
159900010601
160000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160100010601     C     repfil        BEGSR
160200010601     C*
160300010601     C                   if        invfil = *zeros and
160400010601     C                             depfil > *zeros and
160500010629     C                             (vinflg = *blanks or
160600010629     C                              vinflg = *zeros)
160700010601     C
160800010601     C                   eval      invfil = depfil
160900010601     C                   endif
161000010601     C*
161100010601     C                   if        depfil <> invfil and
161200010601     C                             invfil > *zeros
161300010601     C                   eval      flgMulti = '1'
161400010601     C                   if        vinflg = *blanks
161500010601     C                   add       1             cntNonEl
161600010601     C                   endif
161700010601     C                   endif
161800010601     C*
161900010601     C                   if        vinflg = '2'
162000010601     C                   eval      flgStato = '2'
162100010601     C                   endif
162200010601     C*
162300010601     C                   ENDSR
162400010601     C***
162500010601
162600010601
162700010601
162800010330
162900040119      /TITLE Invio dei dati al punto operativo.
163000040119     C     invio         BEGSR
163100040119     C*
163200040119     C* 1° invio FIVAT
163300040119     C                   reset                   dscmz
163400040119     C                   move      vlrpoi        cmzdst
163500040119     C                   eval      cmzfld = 'FIVATWWR'
163600040119     C                   eval      cmzmbd = vlrhdl
163700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163800040119     C***                if        prmfir = *blanks
163900040119     C                   eval      cmzfla = 'FIVAT00F'
164000040119     C                   eval      cmzmba = 'FIVAT00F'
164100040119     C***                else
164200040119     C***                eval      cmzfla = prmfir
164300040119     C***                eval      cmzmba = prmfir
164400040119     C***                endif
164500040119     C                   eval      cmznrr = *zeros
164600040119     C                   move      §ctrokvt      cmznrr
164700040119     C                   eval      cmzlba = vlrfl1
164800040119     C                   call(e)   'TIS711C'
164900040119     C                   parm                    dscmz
165000040119     C                   parm      *blanks       esito
165100040119     C                   if        %error
165200040119     C                             or cmzerr = '1'
165300040119     C                             or esito  = '1'
165400040119     C                   eval      wrkesito = '3'
165500040119     C                   else
165600040119     C*
165700040119     C* 2° invio FIVAB
165800040119     C                   reset                   dscmz
165900040119     C                   move      vlrpoi        cmzdst
166000040119     C                   eval      cmzfld = vlrfou
166100040119     C                   eval      cmzmbd = vlrhdl
166200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166300040119     C***                if        prmfir = *blanks
166400040119     C                   eval      cmzfla = 'FIVAB00F'
166500040119     C                   eval      cmzmba = 'FIVAB00F'
166600040119     C***                else
166700040119     C***                eval      cmzfla = prmfir
166800040119     C***                eval      cmzmba = prmfir
166900040119     C***                endif
167000040119     C                   eval      cmznrr = *zeros
167100040119     C                   move      §ctrokvb      cmznrr
167200040119     C                   eval      cmzlba = vlrfl1
167300040119     C                   call(e)   'TIS711C'
167400040119     C                   parm                    dscmz
167500040119     C                   parm      *blanks       esito
167600040119     C                   if        %error
167700040119     C                             or cmzerr = '1'
167800040119     C                             or esito  = '1'
167900040119     C                   eval      wrkesito = '3'
168000040119     C                   endif
168100040119     C                   endif
168200040119     C*
168300040119     C                   ENDSR
168400040119     C***
168500010601
168600010601
168700010601
168800010601
168900010601      /TITLE Invio dei dati al punto operativo.
169000010601     C     opeini        BEGSR
169100010601     C*
169200010601     C* Inizializzo flag e contatori operativi
169300010601     C                   movel     '0'           flgGiro           1
169400010601     C                   movel     '0'           flgMulti          1
169500010601     C                   movel     '1'           flgStato          1
169600010615     C                   movel     '0'           flgOk             1
169700010601     C                   z-add     *zeros        cntNonEl         10 0
169800010601     C                   z-add     *zeros        depfil            3 0
169900010601     C                   z-add     *zeros        invfil            3 0
170000010601     C*
170100010601     C                   ENDSR
170200010601     C***
170300070326
170400070326
170500070326
170600070326
170700070326     C     *pssr         BEGSR
170800070329     C*
170900070329     C                   if        %open(tivin00r)
171000070329     C                   close     tivin00r
171100070329     C                   endif
171200070329     C                   if        %open(fivabwwr)
171300070329     C                   close     fivabwwr
171400070329     C                   endif
171500070329     C                   if        %open(fivatwwr)
171600070329     C                   close     fivatwwr
171700070329     C                   endif
171800070326     C*
171900070326     C* Effettuo la chiamata al CLLE preposto
172000070326     C                   call(e)   'TITVVTC'
172100070326     C                   parm                    parccm
172200070326     C                   parm                    parmbr
172300070326     C                   parm      '2'           paropz
172400070326     C*
172500070326     C                   eval      wrkesito = '2'
172600070404     C*
172700070404     C                   seton                                        LR
172800070326     C*
172900070326     C                   ENDSR     '*CANCL'
173000070326     C***
173100100318
173200100318
173300100318
173400100318     C     normCSV       BEGSR
173500100318     C*
173600100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173700100318     C* separatori campi
173800100318     C                   if        CharTXT <> *blanks
173900100318     C                   z-add     *zeros        wLenVINDTA        4 0
174000100318     C                   z-add     *zeros        wFlgVINDTA        1 0
174100100318     C                   setoff                                       80
174200100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
174300100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
174400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
174500100318     C                   if        not *in80
174600100318     C                   seton                                        80
174700100318     C                   else
174800100318     C                   setoff                                       80
174900100318     C                   endif
175000100318     C                   endif
175100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
175200100318     C                             and *in80 = *on
175300100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
175400100318     C                   endif
175500100318     C                   enddo
175600100318     C                   endif
175700100318     C*
175800100318     C                   ENDSR
175900100318     C***
176000070326
176100100326
176200100326
176300100326     C*----------------------------------------------------*
176400100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
176500100326     C*----------------------------------------------------*
176600100326     C     REPNSP        BEGSR
176700100326     C*
176800100326     C* NSP => Stacco un numeratore da AZNUM
176900100326     C                   clear                   TRUL33DS
177000100326     C                   eval      I33OPE = *zeros
177100100326     C                   eval      I33CNU = 302
177200100326     C                   eval      I33NUM = 1
177300100326     C                   movel     TRUL33DS      KPJBU
177400100326     C                   call      'TRUL33R'
177500100326     C                   parm                    KPJBA
177600100326     C                   movel     KPJBU         TRUL33DS
177700100326     C                   if        O33ERR = *zeros
177800100326     C                   z-add     O33NRF        VABNSP
177900100326     C                   z-add     O33NRF        VATNSP
178000100326     C                   else
178100100326     C                   SETON                                        31
178200100326     C                   EVAL      vinmsg = %trimr(vinmsg)
178300100326     C                             + ' ' + 'VABNSP VATNSP'
178400100326     C                   endif
178500100326     C*
178600100326     C                   ENDSR
178700010330
178800100326
178900010330
179000000613     C     *inzsr        BEGSR
179100990910     C*
179200990910     C     *entry        plist
179300990920     C                   parm                    tivlrds
179400990921     C                   parm      wrkesito      esito
179500000724     C                   parm                    prmlit
179600000710     C                   parm                    prmfir
179700010330     C*
179800010330     C* CALCOLA LA DATA CORRENTE
179900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
180000100318     C                   eval      datcor = %dec(%date() : *ISO)
180100000613     C*
180200000613     C                   ENDSR
180300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
180400130610W_VATNOT_E;VATNOT_A;VABRSD;VABIND;VABLOD;VABCAD;VATNOT_B;VABCAS;VABPKB;VABVLB;VABNCL;
180500130614W_VABNOT;VABTIC;
180600130528
180700111116
180800100318
