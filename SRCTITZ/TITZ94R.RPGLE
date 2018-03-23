000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400130618      * Disk A
000500130618      * - Il cliente trasmette un record per articolo ma le spedizioni sono sempre monocollo occorre
000600130618      * quindi considerare un sono record a parità di numero di ordine
000700130618      * => creare spedizione e dettagli (FNVAB e FNVAT) solo sul primo record a rottura di codice
000800130618      * sul valore della prima colonna "ORDINE_ID"
000900130618      * - Le spedizioni sono sempre in porto franco o franco con contrassegno
001000130618      * => se nel campo "METODO_PAGAMENTO_CONTRASSEGNO" è presente  la dicitura Contrassegno la
001100130618      * spedizione è da considerarsi in contrassegno
001200130618      * - Il cliente non trasmette la sigla della provincia
001300130618      * => calcolarla tramite TISI95R
001400130625      *
001500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001600120209     F*
001700990910     Ftivin00r  uF   E             DISK    usropn
001800100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001900040119     FFIVATwwr  O    E             DISK    usropn
002000000313     D*
002100040113     D*------------
002200040113     D* SCHIEDE A PROGRAMMA
002300040113     D*------------
002400080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002500010330     D*----------------------------------------------------
002600010330     D* DICHIARAZIOINE VARIABILI DI WRK
002700010330     D*----------------------------------------------------
002800010330     D dscmz         e ds                  inz
002900010330     D psds           sds
003000010330     D  procname         *PROC
003100010330     D tivlrds       e ds                  extname(tivlr00f)
003200040128     D tisi95ds      e ds
003300100413     D fivabds       e ds                  extname(fivab00f)
003400100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003500100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003600010330     D esito           s              1
003700010330     D prmlit          s             10
003800010330     D prmfir          s             10
003900010330     D wrkesito        s                   like(esito)
004000010330     D rrnum           s              6  0 INZ(*zeros)
004100040510     D parccm          s              8    INZ(*blanks)
004200040119     D parmbr          s             10    INZ(*blanks)
004300040119     D paropz          s              1    INZ(*blanks)
004400040119     D chkcall         s              1    INZ(*blanks)
004500080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004600080418     D wFlgCAS         s              1    INZ(*blanks)
004700120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004800120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004900130618     D Intestazioni    s               n   inz(*on)
005000100326     D*------------------
005100100326     D* DS REPERIMENTO NUMERATORE
005200100326     D*------------------
005300100326     D trul33ds      e ds                  inz
005400100326     D*------------------
005500100326     D* DS ARCHITETTURA
005600100326     D*------------------
005700100326     D kpjba         e ds                  inz
005800130611     D*------------------
005900130611     D TRUL28DSX     e ds                  inz
006000100326
006100030715     D*------------------
006200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006300030715     D*------------------
006400030715     D SkSplitFLD      S             10    DIM(100)
006500030715     D SkSplitCSV      S            256    DIM(100)
006600030715     D CharCSV         S              1
006700030715     D CharTXT         S              1
006800030715     D CharNUM         S              1
006900100318     D CharSOS         S              1
007000130626     D posDa           S              4  0 INZ(*zeros)
007100130626     D posA            S              4  0 INZ(*zeros)
007200030715     D i               s              3  0 INZ(1)
007300030715     D wGiro           s              1  0 INZ(*zeros)
007400070823     D wVATNOT_A       s                   LIKE(VATNOT)
007500060331     D wVATNOT_B       s                   LIKE(VATNOT)
007600060331     D wVATNOT_E       s                   LIKE(VATNOT)
007700110927     D wVATNOT_I       s                   LIKE(VATNOT)
007800120411     D wNOT            s             70
007900120411     D Data8A          s              8
008000030822     D*------------------
008100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008200030822     D*------------------
008300030822     D posDaDft        S              3  0 INZ(*zeros)
008400030822     D posADft         S              3  0 INZ(*zeros)
008500030822     D j               s              3  0 INZ(1)
008600030822     D wGiroDft        s              1  0 INZ(*zeros)
008700080418     D*------------------
008800080418     D* Costanti
008900080418     D*------------------
009000080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009100080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009200100318
009300100318     D*------------------
009400100318     D* LINKING A DEFINIZIONI ESTERNE
009500100318     D*------------------
009600100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009700100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009800100318
009900010330
010000010330
010100010330
010200000913     C                   reset                   rrnum
010300990921     C                   reset                   esito
010400990921     C                   reset                   wrkesito
010500010601     C*
010600010601     C                   exsr      opeini
010700010605     C                   exsr      rwvab
010800010601     C*
010900010601     C                   seton                                        lr
011000010601
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011500010601     C*--------------------------------------------------------
011600010601     C     PREELA        BEGSR
011700010601     C*
011800010601     C* SE OCCORRE SPEDIRE IN FILIALE
011900010601     C                   if        invfil <> *zeros and
012000010601     C                             flgGiro = '0'
012100010601     C*
012200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012300010601     C                   eval      flgGiro = '1'
012400010601     C*
012500010601     C                   endif
012600010601     C*
012700010601     C                   ENDSR
012800010601     C***
012900010601
013000010601
013100010601
013200010601
013300010601     C*--------------------------------------------------------
013400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013500010601     C*--------------------------------------------------------
013600010601     C     ENDELA        BEGSR
013700040119     C*
013800040119     C* Effettuo la chiamata al CLLE preposto
013900040119     C                   call(e)   'TITVVTC'
014000040510     C                   parm                    parccm
014100040119     C                   parm                    parmbr
014200040119     C                   parm      '2'           paropz
014300050201     C*
014400050201     C* Effettuo lancio TISI95 solo x chiusura
014500050201     C                   CLEAR                   TISI95DS
014600050201     C                   EVAL      I95TLA = 'C'
014700050201     C                   CALL      'TISI95R'
014800050201     C                   PARM                    TISI95DS
014900000616     C*
015000010601     C                   ENDSR
015100010601
015200010601
015300010330     C*--------------------------------------------------------
015400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015500010330     C*--------------------------------------------------------
015600010605     C     RWVAB         BEGSR
015700010330     C*
015800010330     C                   if        not %open(tivin00r)
015900010330     C                   open      tivin00r
016000010330     C                   endif
016100021025     C                   if        not %open(fivabwwr)
016200021025     C                   open      fivabwwr
016300010330     C                   endif
016400040119     C*
016500040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016600040119     C                   exsr      prevat
016700040119     C*
016800040119     C                   if        chkcall = '0'
016900040119     C*
017000040119     C                   if        not %open(fivatwwr)
017100040119     C                   open      fivatwwr
017200040119     C                   endif
017300010330     C*
017400040119     C                   clear                   §CTROKVB          7 0
017500040119     C                   clear                   §CTROKVT          7 0
017600040119     C                   clear                   §CTRMO            7 0
017700040119     C                   clear                   §CTRNO            7 0
017800120410     C*
017900120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018000120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018100120410     C                   IF        Intestazioni = *off
018200120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018300120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018400120410     C                   exsr      inzvar
018500120410     C                   exsr      impvab
018600120410     C                   ENDIF
018700010330     C*
018800030822     C                   do        *HIVAL
018900010330     C*
019000030822     C                   read      tivin00r                               70
019100010618     C*
019200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019300010618     C                   if        vinflg = '1'
019400010618     C                   eval      flgOk = '1'
019500010618     C                   endif
019600040510     C*
019700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019800040510     C                   z-add     *zeros        wLenVINDTA        4 0
019900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020300040510     C                   z-add     1             wFlgVINDTA
020400040510     C                   leave
020500040510     C                   endif
020600040510     C                   enddo
020700010618     C*
020800040510     C                   if        vindta > *blanks AND
020900040510     C                             wFlgVINDTA = 1
021000010330     C                   add       1             rrnum
021100010330     C*
021200010601     C                   if        *in70 = *off and
021300010330     C                             (vinflg = *blanks
021400010330     C                              or vinflg = '0'
021500010330     C                              or vinflg = '2')
021600010330     C*
021700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021800010711     C                   if        vinflg = *blanks or vinflg = '0'
021900010711     C                   clear                   vinmsg
022000010711     C                   endif
022100010601     C*
022200070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022300130605     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
022400130605     C* evitare che vengano elisi i blank
022500130610     C                   eval      vindta  = ' ' + %trim(vindta)
022600070925     C*
022700080527     C                   exsr      inzvar
022800100318     C                   exsr      normCSV
022900010605     C                   exsr      impvab
023000010601     C*
023100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023200010605     C                   eval      depfil = VABLNP
023300010601     C                   exsr      repfil
023400010601     C                   if        depfil = invfil
023500021025     C                   if        vlrpoi = 999
023600030822     C                   move(P)   invfil        VABFGS
023700021025     C                   else
023800030822     C                   move(P)   vlrpoi        VABFGS
023900021025     C                   endif
024000010601     C*
024100010601     C                   exsr      PREELA
024200010601     C*
024300010604     C* Ebbene...
024400010604     C*
024500040119     C  N31              add       1             §CTROKVB
024600040119     C   32              add       1             §CTRMO
024700040119     C   31              add       1             §CTRNO
024800080418     C*
024900030822     C                   if        wGiro = 2
025000080513     C* Scrivo sempre il VAT
025100080513     C  N31              exsr      wrivat
025200110201     C                   exsr      chkvab
025300080513     C                   endif
025400010604     C*
025500010604     C                   if        *in31 = *off and
025600010604     C                             *in32 = *off
025700010604     C                   eval      vinflg = '1'
025800010604     C                   else
025900010604     C                   eval      vinflg = '2'
026000010604     C                   endif
026100010604     C                   endif
026200010601     C*
026300010604     C                   endif
026400010604     C*
026500010330     C                   else
026600010330     C                   eval      vinflg = '1'
026700010330     C                   endif
026800010601     C*
026900010601     C  N70              update    tivin000
027000010330     C*
027100030822     C  N70              enddo
027200040119     C*
027300080527     C* Verifico dati rimasti "in cannna"...
027400080527     C                   seton                                        65
027500110201     C                   exsr      chkvab
027600080527     C*
027700040119     C                   endif                                                  (endif - chkcall)
027800010601     C*
027900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028000010601     C                   if        cntNonEl = *zeros or
028100010601     C                             flgMulti = '0'
028200010330     C* Se non ci sono record con errori ...
028300010601     C                   if        §ctrno = 0 and
028400010604     C                             §ctrmo = 0 and
028500010601     C                             flgStato <> '2'
028600010330     C* ... restituisco esito OK.
028700010330     C                   eval      wrkesito = '0'
028800010330     C                   else
028900040119     C                   if        §ctrokvb > 0
029000010330     C                   eval      wrkesito = '1'
029100010330     C                   else
029200010615     C                   if        flgOk = '0'
029300010615     C                   eval      wrkesito = '2'
029400010615     C                   else
029500010615     C                   eval      wrkesito = '6'
029600010615     C                   endif
029700010330     C                   endif
029800010330     C                   endif
029900010601     C                   else
030000010601     C                   eval      wrkesito = '9'
030100010601     C                   endif
030200010330     C*
030300010330     C                   if        %open(tivin00r)
030400010330     C                   close     tivin00r
030500010330     C                   endif
030600021025     C                   if        %open(fivabwwr)
030700021025     C                   close     fivabwwr
030800010330     C                   endif
030900040512     C                   if        %open(fivatwwr)
031000040512     C                   close     fivatwwr
031100040512     C                   endif
031200010601     C*
031300010601     C                   if        vlrpoi <> 999
031400010601     C                   eval      invfil = vlrpoi
031500010601     C                   endif
031600010330     C*
031700040119     C                   if        §ctrokvb > 0
031800010601     C                             and invfil > *zeros
031900010330     C                   exsr      invio
032000010330     C                   endif
032100010601     C*
032200010618     C                   if        flgGiro = '1'
032300010601     C                   exsr      endela
032400010618     C                   endif
032500010330     C*
032600010330     C                   ENDSR
032700010330     C***
032800080527
032900080527
033000080527
033100080527     C*----------------------------------------------------*
033200080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033300080527     C*----------------------------------------------------*
033400100413     C     CHKVAB        BEGSR
033500110201     C*
033600110201     C* Se richiesti ignorare colli / peso / volume
033700110315 xxx C***                if        *in35
033800110315 xxx C***                z-add     *zeros        VABNCL
033900110315 xxx C***                z-add     *zeros        VABPKB
034000110315 xxx C***                z-add     *zeros        VABVLB
034100110315 xxx C***                endif
034200080527     C*
034300080527     C* Scrivo il VAB solo a rottura d spedizione
034400080527     C                   if        depspe = *zeros
034500080527     C  N31              eval      fivabds_sav = fivabds
034600080527     C                   eval      depspe = vabNSP
034700080527     C                   else
034800080527     C                   if        depspe = vabNSP AND *in65 = *off
034900100413     C   51              add       VABNCL        sav_VABNCL
035000100413     C   51              add       VABPKB        sav_VABPKB
035100100413     C   51              add       VABVLB        sav_VABVLB
035200100720     C   52              z-add     VABNCL        sav_VABNCL
035300100720     C   52              z-add     VABPKB        sav_VABPKB
035400100720     C   52              z-add     VABVLB        sav_VABVLB
035500100720     C   53              add       VABNCL        sav_VABNCL
035600100720     C   53              z-add     VABPKB        sav_VABPKB
035700100720     C   53              z-add     VABVLB        sav_VABVLB
035800080527     C                   else
035900080527     C  N31              eval      fivabds_ok = fivabds_sav
036000080527     C  N31              exsr      wrivab
036100080527     C  N31              eval      fivabds_sav = fivabds
036200080527     C                   eval      depspe = vabNSP
036300080527     C                   endif
036400080527     C                   endif
036500080527     C*
036600080527     C                   ENDSR
036700040119
036800040119
036900040119
037000040119     C*----------------------------------------------------*
037100040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037200040119     C*----------------------------------------------------*
037300040510     C     WRIVAB        BEGSR
037400080513     C*
037500080513     C* Gestisco l'eventuale rottura x numero spedizione
037600080527     C                   if        VABNSP <> depspe or *in65 = *on
037700070301     C                   if        wVATNOT_E <> *blanks
037800130610     C                   eval      ok_VABCTM = '7Q'
037900070301     C                   endif
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
039700060512     C                   eval      VATNSP = VABNSP
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
042400120320     C*
042500120410     C* i rcd di FIVAT con tipo E sono 1 in quanto è una spedizione monocollo
042600060331     C                   if        wVATNOT_E <> *blanks
042700110201     C                   eval      VATNOT = wVATNOT_E
042800060331     C                   eval      VATTRC = 'E'
042900120411     C*
043000120411     C* Sviluppati i "CHI SONO" in relazione al numero colli
043100120411     C*
043200120411     C     1             DO        VABNCL        wNumCollo         3 0
043300130611     C*** da analisi del POC il progressivo colli è max 3 char e deve contenere gli zeri non
043400120411     C*** significativi
043500120411     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
043600120411     C                                %editc(wNumCollo:'X')
043700130611     C* recupero il check digit
043800130611     C                   clear                   TRUL28DSX
043900130611     C                   eval      I284STD = 'E10'
044000130611     C                   eval      I284COD = VATNOT
044100130611     C                   call      'TRUL28R4'
044200130611     C                   parm                    TRUL28DSX
044300130611     C* Se tutto ok scrivo il "chi sono" corrente col check digit
044400130611     C                   IF        O284err = *blanks
044500130611     C                   EVAL      VATNOT = %trim(VATNOT)
044600130611     C                              + %trim(O284CkD)
044700120410     C                   write     FIVAT000
044800120410     C                   add       1             §CTROKVT
044900130611     C                   ELSE
045000130611     C                   SETON                                        31
045100130611     C                   EVAL      VATNOT = *blank
045200130611     C                   EVAL      vinmsg = %trimr(vinmsg)
045300130611     C                             + ' ' + 'VATNOT'
045400130611     C                   ENDIF
045500120411     C                   ENDDO
045600120320     C*
045700120320     C                   endif
045800120320     C*
045900110927     C*
046000110927     C                   if        wVATNOT_I <> *blanks
046100110927     C                   eval      VATNOT = wVATNOT_I
046200110927     C                   eval      VATTRC = 'I'
046300110927     C                   write     FIVAT000
046400110927     C                   add       1             §CTROKVT
046500110927     C                   endif
046600040119     C*
046700040119     C                   ENDSR
046800040119
046900040119
047000040119
047100040119     C*----------------------------------------------------*
047200040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047300040119     C*----------------------------------------------------*
047400040119     C     PREVAT        BEGSR
047500040119     C*
047600040119     C* Compongo il nome del membro da dare al FIVATWWR
047700040119     C                   eval      parmbr = vlrhdl
047800040119     C                   movel     'M'           parmbr
047900040510     C                   eval      parccm = vlrksc
048000040119     C                   eval      paropz = '1'
048100040119     C* Effettuo la chiamata al CLLE preposto
048200040119     C                   call(e)   'TITVVTC'
048300040119     C                   parm                    parccm
048400040119     C                   parm                    parmbr
048500040119     C                   parm                    paropz
048600040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048700040119     C                   if        %error
048800040119     C                   movel     '1'           chkcall
048900040119     C                   else
049000040119     C                   movel     '0'           chkcall
049100040119     C                   endif
049200040119     C*
049300040119     C                   ENDSR
049400010601
049500010601
049600010601
049700010330     C*----------------------------------------------------*
049800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049900010330     C*----------------------------------------------------*
050000040119     C     INZVAR        BEGSR
050100080527     C*
050200080527     C                   setoff                                       65
050300010330     C*
050400030822     C* Inizializzo variabili di wrk
050500020204     C                   Z-ADD     *zeros        Num5_0            5 0
050600101018     C                   MOVEL     *blanks       wFlgCAS
050700060331     C                   MOVEL     *blanks       wVATNOT_A
050800060331     C                   MOVEL     *blanks       wVATNOT_B
050900060331     C                   MOVEL     *blanks       wVATNOT_E
051000110927     C                   MOVEL     *blanks       wVATNOT_I
051100120411     C                   MOVEL     *blanks       wNOT
051200120411     C                   MOVEL     *blanks       Data8A
051300030822     C*
051400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051500030822     C                   CLEAR                   SkSplitCSV
051600030822     C                   EVAL      i = 1
051700030822     C                   EVAL      posDa = *zeros
051800030822     C                   EVAL      posA  = *zeros
051900080527     C*
052000080527     C                   CLEAR                   FIVABDS
052100080527     C                   CLEAR                   FIVAT000
052200030822     C*
052300030822     C* Reimposto i valori di default
052400030822     C                   EXSR      DEFCAM
052500030822     C*
052600010330     C                   ENDSR
052700010330     C*----------------------------------------------------*
052800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052900010330     C*----------------------------------------------------*
053000010330     C     DEFCAM        BEGSR
053100030822     C*
053200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
053300030715     C* e delimitatore testo.
053400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
053700100318     C*
053800100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
053900100318     C                   EVAL      CharSOS = CharNUM
054000030822     C*
054100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
054200100720     C                   SETOFF                                       505152
054300100720     C                   SETOFF                                       53
054400100413     C                   SELECT
054500100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
054600100720     C                   SETON                                        50
054700100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
054800100413     C                   SETON                                        51
054900100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
055000100720     C                   SETON                                        52
055100100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
055200100720     C                   SETON                                        53
055300100413     C                   ENDSL
055400030822     C*
055500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055600030822     C                   EVAL      posDaDft = 1
055700030822     C                   EVAL      posADft  = 0
055800030822     C                   EVAL      wGiroDft = 0
055900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056000030822     C                             posDaDft > 0
056100030822     C*
056200030822     C* Gestisco il 1° giro
056300030822     C                   IF        wGiroDft = 0
056400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056600030822     C* Incremento il contatore dei "giri"
056700030822     C                   EVAL      wGiroDft = 1
056800030822     C                   ELSE
056900030822     C                   EVAL      posDaDft = posADft
057000030822     C                   ENDIF
057100030822     C* Eseguo lo scan x trovare la fine del campo corrente
057200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
057300030822     C*
057400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057600030822     C                   IF        posDaDft > 0 AND
057700030822     C                             posADft  > 0
057800030822     C* NCL
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'NCL'
058300030822     C                   EVAL      PiStr=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   EXSR      CHKNUM
058700030822     C                   IF        PiInt=*on
058800030822     C                   Z-ADD     PiVal         VABNCL
058900030822     C                   ENDIF
059000030822     C                   ENDIF
059100030822     C* CCM
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'CCM'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABCCM
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* LNP
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'LNP'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiInt=*on
061400030822     C                   Z-ADD     PiVal         VABLNP
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* NRS
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'NRS'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiInt=*on
062700030822     C                   Z-ADD     PiVal         VABNRS
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* CTR
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'CTR'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiInt=*on
064000030822     C                   Z-ADD     PiVal         VABCTR
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* PKB
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'PKB'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiNum=*on
065300130610     C                   Z-ADD     PiVal         VABPKB
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* VLB
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'VLB'
066100030822     C                   EVAL      PiStr=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   EXSR      CHKNUM
066500030822     C                   IF        PiNum=*on
066600130610     C                   Z-ADD     PiVal         VABVLB
066700030822     C                   ENDIF
066800030822     C                   ENDIF
066900030822     C* QFT
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'QFT'
067400030822     C                   EVAL      PiStr=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   EXSR      CHKNUM
067800030822     C                   IF        PiNum=*on
067900030822     C                   Z-ADD     PiVal         VABQFT
068000030822     C                   ENDIF
068100030822     C                   ENDIF
068200030822     C* CBO
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'CBO'
068700030822     C                   EVAL      VABCBO=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* TSP
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'TSP'
069600030822     C                   EVAL      VABTSP=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* VAS
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'VAS'
070500030822     C                   EVAL      VABVAS=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* VCA
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'VCA'
071400030822     C                   EVAL      VABVCA=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800070925     C* TIC
071900070925     C                   IF        %subst(
072000070925     C                             %subst(vlrppt:posDaDft+1:
072100070925     C                             posADft-posDaDft-1):1:3)
072200070925     C                             = 'TIC'
072300070925     C                   EVAL      VABTIC=%trim(%subst(
072400070925     C                             %subst(vlrppt:posDaDft+1:
072500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600070925     C                   ENDIF
072700030822     C* GCA
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'GCA'
073200030822     C                   EVAL      VABGCA=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* CTM
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'CTM'
074100030822     C                   EVAL      VABCTM=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* FFD
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'FFD'
075000030822     C                   EVAL      VABFFD=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* VAD
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'VAD'
075900030822     C                   EVAL      VABVAD=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* GMA
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'GMA'
076800030822     C                   EVAL      VABGMA=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* GGA
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'GGA'
077700030822     C                   EVAL      VABGGA=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* GVA
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'GVA'
078600030822     C                   EVAL      VABGVA=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* TC1
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'TC1'
079500030822     C                   EVAL      VABTC1=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* TC2
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'TC2'
080400030822     C                   EVAL      VABTC2=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800040714     C* VATTRC
080900040714     C                   IF        %subst(
081000040714     C                             %subst(vlrppt:posDaDft+1:
081100040714     C                             posADft-posDaDft-1):1:3)
081200040714     C                             = 'TRC'
081300040714     C                   EVAL      VATTRC=%trim(%subst(
081400040714     C                             %subst(vlrppt:posDaDft+1:
081500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600040714     C                   ENDIF
081700030822     C* ...
081800030822     C                   ENDIF
081900030822     C                   ENDDO
082000020204     C*
082100010330     C                   ENDSR
082200010607     C*----------------------------------------------------*
082300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
082400010607     C*----------------------------------------------------*
082500010607     C     IMPVAB        BEGSR
082600010607     C*
082700010607     C                   SETOFF                                       3132
082800110315 xxx C***                SETON                                        35
082900030515     C*
083000010607     C                   MOVEL     datcor        VABAAS
083100010607     C                   MOVE      datcor        VABMGS
083200010607     C*
083300030715     C**********
083400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083500030715     C**********
083600030715     C                   IF        wGiro = *zeros
083700040113     C*
083800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
083900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
084100080429     C                             %trim(intcol(5))<>*blanks
084200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
084300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
084400080429     C                                      %trim(intcol(5))
084500070823     C     ';':CharCSV   XLATE     vindta        vindta
084600040510     C                   ENDIF
084700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084800031201     C     minu:maiu     XLATE     vindta        vindta
084900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085000030715     C                   DOW       posDa <= %len(%trim(vindta))
085100030715     C*
085200030715     C* Gestisco il 1° campo
085300030715     C                   IF        i = 1
085400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085500030715     C                   EVAL      posDa = 1
085600030715     C* Eseguo lo scan x trovare la fine del primo campo
085700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085800030715     C* A questo punto "estraggo" il campo corrente
085900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086000030715     C                                                    (posA-posDa))
086100030715     C* X i campi successivi al 1°
086200030715     C                   ELSE
086300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086400030715     C                   EVAL      posDa = posA + 1
086500030715     C* Eseguo lo scan x trovare la fine del campo corrente
086600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086700030715     C* Gestisco l'ultimo campo
086800030715     C                   IF        posA = *zeros
086900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087000030715     C* A questo punto "estraggo" il campo corrente
087100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087200030715     C                                                    (posA-posDa))
087300030715     C                   LEAVE
087400030715     C                   ELSE
087500030715     C* A questo punto "estraggo" il campo corrente
087600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087700030715     C                                                    (posA-posDa))
087800030715     C                   ENDIF
087900030715     C                   ENDIF
088000030715     C* Incremento il contatore d campo
088100030715     C                   EVAL      i = i +1
088200030715     C                   ENDDO
088300030715     C*
088400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088500030715     C* delimitazione testo
088600030715     C                   EVAL      i = 1
088700030715     C                   DOW       i < %elem(SkSplitFLD)
088800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
088900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089000030715     C                   EVAL      i = i + 1
089100030715     C                   ENDDO
089200030715     C                   EVAL      wGiro = 1
089300030715     C                   ELSE
089400030715     C                   EVAL      wGiro = 2
089500031204     C**********
089600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
089700031204     C* d separatore campo
089800031204     C**********
089900031204     C                   EVAL      vindta = %trim(vindta)
090000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090100031204     C                   EVAL      vindta = %subst(vindta:2)
090200031204     C                   ENDDO
090300031204     C*
090400031204     C                   Z-ADD     *zeros        lunghInput        4 0
090500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
090700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
090800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090900110912     C                   ENDDO
091000030715     C**********
091100030715     C* Effettuo lo split del campo dati d input
091200030715     C**********
091300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091400030715     C                   DOW       posDa <= %len(%trim(vindta))
091500030715     C*
091600030715     C* Gestisco il 1° campo
091700030715     C                   IF        i = 1
091800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091900030715     C                   EVAL      posDa = 1
092000030715     C* Eseguo lo scan x trovare la fine del primo campo
092100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092200030715     C* A questo punto "estraggo" il campo corrente
092300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092400030715     C                                                    (posA-posDa))
092500030715     C* X i campi successivi al 1°
092600030715     C                   ELSE
092700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092800030715     C                   EVAL      posDa = posA + 1
092900031204     C* Verifico che nn vi sia il campo nn valorizzato
093000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093100031204     C* Se campo nn valorizzato skippo al prossimo
093200031204     C                   EVAL      SkSplitCSV(i) = *blanks
093300031204     C                   EVAL      posA  = posA + 1
093400031204     C                   ELSE
093500030715     C* Eseguo lo scan x trovare la fine del campo corrente
093600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093700030715     C* Gestisco l'ultimo campo
093800030715     C                   IF        posA = *zeros
093900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094000030715     C* A questo punto "estraggo" il campo corrente
094100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094200030715     C                                                    (posA-posDa))
094300030715     C                   LEAVE
094400030715     C                   ELSE
094500030715     C* A questo punto "estraggo" il campo corrente
094600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094700030715     C                                                    (posA-posDa))
094800030715     C                   ENDIF
094900031204     C                   ENDIF
095000030715     C                   ENDIF
095100030715     C* Incremento il contatore d campo
095200030715     C                   EVAL      i = i +1
095300030715     C                   ENDDO
095400030715     C*
095500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095600030715     C* delimitazione testo
095700030715     C                   EVAL      i = 1
095800030715     C                   DOW       i < %elem(SkSplitCSV)
095900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096100030715     C                   EVAL      i = i + 1
096200030715     C                   ENDDO
096300100720     C*
096400100720     C* Stacco un progressivo x numero spedizione
096500130619 xxx C***                EXSR      repNSP
096600100326     C*
096700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096800030715     C                   EVAL      i = 1
096900030715     C                   DOW       i < %elem(SkSplitFLD)
097000030715     C*
097100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097300030715     C                             SkSplitCSV(i) <> *blanks
097400030721     C*** ATB
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097600030721     C                   EVAL      VABATB = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030715     C*** RSD
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098300030715     C* ==
098400130528     C                   ENDIF
098500030721     C*** RD2
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
098700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030715     C*** IND
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099100030715     C                   EVAL      VABIND = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030715     C*** LOD
099400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
099600030715     C                   ENDIF
099700030715     C*** PRD
099800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100000030715     C                   ENDIF
100100030721     C*** NZD
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030715     C*** RMA
100600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
100700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
100800030715     C                   ENDIF
100900030715     C*** NOT
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101200030715     C                   ENDIF
101300030715     C*** NT2
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** VCA
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030715     C*** VAS
102200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102400030715     C                   ENDIF
102500030715     C*** FFD
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
102700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030721     C*** GC1
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** GC2
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** TSP
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104000030721     C                   ENDIF
104100040510     C*** CBO
104200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
104400040510     C                   ENDIF
104500030721     C*** NAS
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
104700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** TIC
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** GCA
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
105500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** XCO
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** CTM
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
106300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** TCR
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
106700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** CTS
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** FTM
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
107500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** VAD
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** GMA
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
108300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** GGA
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
108700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** GVA
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** TC1
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
109500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** TC2
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** SCL
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
110300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** RMO
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
110700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** NMO
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*
111400030721     C* Reperisco quindi i campi numerici...
111500040510     C*** NSP
111600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
111700040510     C                   EVAL      PiStr=SkSplitCSV(i)
111800040510     C                   EXSR      CHKNUM
111900040510     C                   IF        PiInt=*on
112000040510     C                   Z-ADD     PiVal         VABNSP
112100040510     C                   ELSE
112200040510     C                   SETON                                        31
112300040510     C                   EVAL      VABNSP = *zeros
112400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
112500040510     C                             + ' ' + 'VABNSP'
112600040510     C                   ENDIF
112700040510     C                   ENDIF
112800071106     C*** RMN
112900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113000071106     C                   EVAL      PiStr=SkSplitCSV(i)
113100071106     C                   EXSR      CHKNUM
113200071106     C                   IF        PiInt=*on
113300071106     C                   Z-ADD     PiVal         VABRMN
113400071106     C                   ELSE
113500071106     C                   SETON                                        32
113600071106     C                   EVAL      VABRMN = *zeros
113700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
113800071106     C                             + ' ' + 'VABRMN'
113900071106     C                   ENDIF
114000071106     C                   ENDIF
114100030721     C*** CAD
114200130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
114300130610     C                   EVAL      PiStr=SkSplitCSV(i)
114400130610     C                   EXSR      CHKNUM
114500130610     C                   IF        PiInt=*on
114600130610     C                   Z-ADD     PiVal         Num5_0
114700130610     C                   MOVEL(p)  Num5_0        VABCAD
114800130610     C                   ELSE
114900130610     C                   SETON                                        32
115000130610     C                   EVAL      VABCAD = *zeros
115100130610     C                   EVAL      vinmsg = %trimr(vinmsg)
115200130610     C                             + ' ' + 'VABCAD'
115300130610     C                   ENDIF
115400130605     C                   ENDIF
115500030721     C*** NCL
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on
116000030721     C                   Z-ADD     PiVal         VABNCL
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      VABNCL = *zeros
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500040122     C                             + ' ' + 'VABNCL'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** PKB
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiNum=*on
117300130610     C                   Z-ADD(H)  PiVal         VABPKB
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      VABPKB = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABPKB'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** CAS
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118300110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
118400111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
118500111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
118600130703     C                   EVAL      VABVCA = 'EUR'
118700101018     C                   ENDIF
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiNum=*on
119100030721     C                   Z-ADD     PiVal         VABCAS
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      VABCAS = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABCAS'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** CCM
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on      AND
120400030721     C                             PiVal<=9999999 AND
120500030721     C                             PiVal>*zeros
120600030721     C                   Z-ADD     PiVal         VABCCM
120700030721     C                   ELSE
120800030721     C                   SETON                                        32
120900030721     C                   EVAL      VABCCM = *zeros
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100040510     C                             + ' ' + 'VABCCM'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** LNP
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on    AND
121900030721     C                             PiVal<=999   AND
122000030721     C                             PiVal>*zeros
122100030721     C                   Z-ADD     PiVal         VABLNP
122200030721     C                   ELSE
122300040715     C                   SETON                                        32
122400030721     C                   EVAL      VABLNP = *zeros
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600040510     C                             + ' ' + 'VABLNP'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** NRS
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on AND
123400030721     C                             PiVal<=99
123500030721     C                   Z-ADD     PiVal         VABNRS
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABNRS'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** LNA
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on    AND
124700030721     C                             PiVal<=999   AND
124800030721     C                             PiVal>*zeros
124900030721     C                   Z-ADD     PiVal         VABLNA
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      VABLNA = *zeros
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABLNA'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** CTR
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on  AND
126200030721     C                             PiVal<=999
126300030721     C                   Z-ADD     PiVal         VABCTR
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABCTR'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** IAS
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiNum=*on
127500030721     C                   Z-ADD     PiVal         VABIAS
127600030721     C                   ELSE
127700030721     C                   SETON                                        32
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABIAS'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** VLB
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128400030721     C                   EVAL      PiStr=SkSplitCSV(i)
128500030721     C                   EXSR      CHKNUM
128600030721     C                   IF        PiNum=*on
128700130610     C                   Z-ADD(H)  PiVal         VABVLB
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABVLB'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** QFT
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiNum=*on
129900030721     C                   Z-ADD     PiVal         VABQFT
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABQFT'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** NCD
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiInt=*on      AND
131100030721     C                             PiVal<=9999999
131200030721     C                   Z-ADD     PiVal         VABNCD
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABNCD'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** NCA
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiInt=*on      AND
132400030721     C                             PiVal<=9999999
132500030721     C                   Z-ADD     PiVal         VABNCA
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABNCA'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** ZNC
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiInt=*on AND
133700030721     C                             PiVal<=99
133800030721     C                   Z-ADD     PiVal         VABZNC
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABZNC'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** DCR
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiInt=*on       AND
135000030721     C                             PiVal<=99999999
135100030721     C                   Z-ADD     PiVal         VABDCR
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABDCR'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800030721     C*** HCR
135900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136000030721     C                   EVAL      PiStr=SkSplitCSV(i)
136100030721     C                   EXSR      CHKNUM
136200030721     C                   IF        PiInt=*on   AND
136300030721     C                             PiVal<=9999
136400030721     C                   Z-ADD     PiVal         VABHCR
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABHCR'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100030721     C*** VMD
137200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiNum=*on
137600030721     C                   Z-ADD     PiVal         VABVMD
137700030721     C                   ELSE
137800030721     C                   SETON                                        32
137900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138000030721     C                             + ' ' + 'VABVMD'
138100030721     C                   ENDIF
138200030721     C                   ENDIF
138300030721     C*** ANT
138400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138500030721     C                   EVAL      PiStr=SkSplitCSV(i)
138600030721     C                   EXSR      CHKNUM
138700030721     C                   IF        PiInt=*on        AND
138800030721     C                             PiVal<=999999999
138900030721     C                   Z-ADD     PiVal         VABANT
139000030721     C                   ELSE
139100030721     C                   SETON                                        32
139200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139300030721     C                             + ' ' + 'VABANT'
139400030721     C                   ENDIF
139500030721     C                   ENDIF
139600030721     C*** CMO
139700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139800030721     C                   EVAL      PiStr=SkSplitCSV(i)
139900030721     C                   EXSR      CHKNUM
140000030721     C                   IF        PiInt=*on
140100030721     C                   Z-ADD     PiVal         Num5_0
140200030721     C                   MOVEL(p)  Num5_0        VABCMO
140300030721     C                   ELSE
140400030721     C                   SETON                                        32
140500030721     C                   EVAL      VABCMO = *zeros
140600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140700030721     C                             + ' ' + 'VABCMO'
140800030721     C                   ENDIF
140900030721     C                   ENDIF
141000040510     C*
141100040510     C* Reperisco i campi del FIVAT
141200040510     C*** VATNOT
141300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141500040510     C                   ENDIF
141600040510     C*** VATTRC
141700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141900040510     C                   ENDIF
142000060331     C*** VATNOT_A
142100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142300060331     C                   ENDIF
142400060331     C*** VATNOT_B
142500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142700060331     C                   ENDIF
142800060331     C*** VATNOT_E
142900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143100060331     C                   ENDIF
143200110927     C*** VATNOT_I
143300110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
143400110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
143500110927     C                   ENDIF
143600070823     C*
143700100831     C***  ===>  Gestione campi "particolari" dentro ciclo
143800120301
143900130619      *** W_VABRMN: è sia VABRMA che VABRMN che VABNSP
144000130618     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
144100130618     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
144200130618     C                   EVAL      VABRMN =
144300130618     C                              %dec(VABRMA:15:0)
144400130619     C                   EVAL      VABNSP = VABRMN
144500120411     C                   ENDIF
144600130626
144700130626      *** W_VABNOT = VABNOT + VABNT2
144800130626     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
144900130626     C                   MOVEL     *blanks       wNOT
145000130626     C                   EVAL      wNOT=%trim(SkSplitCSV(i))
145100130626     C                   ENDIF
145200130703
145300130703      *** W_VABTIC
145400130703     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
145500130703     C                   IF        %scan('CONTRASS':%trim(SkSplitCSV(i)):1) > 0
145600130703     C                   EVAL      wFlgCAS = '1'
145700130703     C                   ENDIF
145800130703     C                   ENDIF
145900070925     C*
146000071029     C***  <===  -----------------------------------------
146100040510     C*
146200030715     C                   ENDIF
146300030715     C                   EVAL      i = i + 1
146400030715     C                   ENDDO
146500070925     C*
146600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
146700120411     C*
146800130626     C*** W_VABNOT può contenere sia VABNOT che VABNT2
146900130626     C                   EVAL      VABNOT=%subst(wNOT:1:35)
147000130626     C                   EVAL      VABNT2=%subst(wNOT:36:35)
147100130704     C*
147200130704     C                   IF        wFlgCAS = '1'
147300130704     C                   ELSE
147400130704     C                   Z-ADD     *zeros        VABCAS
147500130704     C                   ENDIF
147600070925     C*
147700071029     C***  <===  -----------------------------------------
147800070925     C*
147900070925     C* Considerazioni finali su CBO/CAS
148000100318     C                   IF        wFlgCAS = '1'    OR
148100100318     C                             VABCAS  > *zeros
148200070925     C                   IF        VABCBO = '1'
148300070925     C                   EVAL      VABCBO = '4'
148400070925     C                   ENDIF
148500070925     C                   IF        VABCBO = '2'
148600070925     C                   EVAL      VABCBO = '6'
148700070925     C                   ENDIF
148800070925     C                   ENDIF
148900070925     C***  <===  ----------------------------
149000040510     C*
149100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149200040510     C                   IF        VABNSP = *zeros
149300040510     C                   SETON                                        31
149400040510     C                   ENDIF
149500040715     C*
149600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149700040715     C                   IF        VABLNP = *zeros
149800040715     C                   SETON                                        31
149900040715     C                   ENDIF
150000040510     C*
150100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150200120411     C                   IF        VABRMN = *zeros
150300120411     C                   Z-ADD     VABNSP        VABRMN
150400120411     C                   ENDIF
150500120328     C* IN QUESTO CASO LO FORZO = 1
150600120411     C***                IF        VABRMN = *zeros
150700120411     C***                Z-ADD     1             VABRMN
150800120411     C***                ENDIF
150900040510     C*
151000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
151100040510     C                   IF        VABRMA = *blanks
151200040714     C                   MOVEL     VABRMN        VABRMA
151300040510     C                   ENDIF
151400040128     C*
151500040510     C* Se provincia nn valorizzata la reperisco
151600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151700110912     C                   IF        VABPRD =  *blanks AND
151800110912     C                             VABNZD =  *blanks AND
151900110912     C                             VABCAD <> *blanks AND
152000110912     C                             VABLOD <> *blanks
152100040128     C                   CLEAR                   TISI95DS
152200040128     C                   EVAL      I95TCN = '3'
152300040128     C                   Z-ADD     datcor        I95DAT
152400040128     C                   EVAL      I95NAR = VABNZD
152500040128     C                   EVAL      I95CAP = VABCAD
152600040128     C                   EVAL      I95LOC = VABLOD
152700040128     C                   CALL      'TISI95R'
152800040128     C                   PARM                    TISI95DS
152900040128     C                   EVAL      VABPRD = O95PRV
153000040510     C                   ENDIF
153100020204     C*
153200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153300020204     C                   EXSR      CHKIMPDIV
153400030715     C                   ENDIF
153500020204     C*
153600010607     C                   ENDSR
153700010607     C*----------------------------------------------------*
153800010601
153900020204
154000020204     C*----------------------------------------------------*
154100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154200020204     C*----------------------------------------------------*
154300020204     C     CHKIMPDIV     BEGSR
154400020204     C*
154500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154600020204     C                   Z-ADD     *zeros        wrkDec            9 9
154700020204     C*
154800020204     C* Come prima cosa effettuo considerazioni sulla divisa
154900020204     C                   IF        vabIAS > *zeros
155000020204     C                   IF        vabVAS <> 'EUR'
155100020204     C                   EVAL      vabVAS =  'ITL'
155200020204     C                   ENDIF
155300020204     C                   ENDIF
155400020204     C*
155500020204     C                   IF        vabCAS > *zeros
155600020204     C                   IF        vabVCA <> 'EUR'
155700020204     C                   EVAL      vabVCA =  'ITL'
155800020204     C                   ENDIF
155900020204     C                   ENDIF
156000020204     C*
156100020204     C                   IF        vabVMD > *zeros
156200020204     C                   IF        vabVAD <> 'EUR'
156300020204     C                   EVAL      vabVAD =  'ITL'
156400020204     C                   ENDIF
156500020204     C                   ENDIF
156600020204     C*
156700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156800020204     C                   Z-ADD     vabIAS        wrkDec
156900020204     C                   IF        wrkDec > *zeros
157000020204     C                   IF        vabVAS = 'ITL'
157100020204     C                   EVAL      vabIAS = *zeros
157200020204     C                   ENDIF
157300020204     C                   ENDIF
157400020204     C*
157500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157600020204     C                   Z-ADD     vabCAS        wrkDec
157700020204     C                   IF        wrkDec > *zeros
157800020204     C                   IF        vabVCA = 'ITL'
157900020204     C                   EVAL      vabCAS = *zeros
158000020204     C                   ENDIF
158100020204     C                   ENDIF
158200020204     C*
158300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158400020204     C                   Z-ADD     vabVMD        wrkDec
158500020204     C                   IF        wrkDec > *zeros
158600020204     C                   IF        vabVAD = 'ITL'
158700020204     C                   EVAL      vabVMD = *zeros
158800020204     C                   ENDIF
158900020204     C                   ENDIF
159000020204     C*
159100020204     C                   ENDSR
159200020204     C***
159300010330
159400100318
159500100318
159600100318     C*----------------------------------------------------*
159700100318     C*  CONTROLLO NUMERICITA' CAMPI
159800100318     C*----------------------------------------------------*
159900100318     C     CHKNUM        BEGSR
160000100318     C*
160100100318     C                   IF        PiDecChr = *blanks
160200100318     C                   EVAL      PiDecChr = CharNUM
160300100318     C                   ENDIF
160400100318     C*
160500100318     C                   callp     UBISNUM_Check(PiStr
160600100318     C                                          :PiDecChr
160700100318     C                                          :PiVal
160800100318     C                                          :PiNum
160900100318     C                                          :PiInt)
161000100318     C*
161100100318     C                   ENDSR
161200100318     C***
161300010601
161400010601
161500010601
161600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161700010601     C     repfil        BEGSR
161800010601     C*
161900010601     C                   if        invfil = *zeros and
162000010601     C                             depfil > *zeros and
162100010629     C                             (vinflg = *blanks or
162200010629     C                              vinflg = *zeros)
162300010601     C
162400010601     C                   eval      invfil = depfil
162500010601     C                   endif
162600010601     C*
162700010601     C                   if        depfil <> invfil and
162800010601     C                             invfil > *zeros
162900010601     C                   eval      flgMulti = '1'
163000010601     C                   if        vinflg = *blanks
163100010601     C                   add       1             cntNonEl
163200010601     C                   endif
163300010601     C                   endif
163400010601     C*
163500010601     C                   if        vinflg = '2'
163600010601     C                   eval      flgStato = '2'
163700010601     C                   endif
163800010601     C*
163900010601     C                   ENDSR
164000010601     C***
164100010601
164200010601
164300010601
164400010330
164500040119      /TITLE Invio dei dati al punto operativo.
164600040119     C     invio         BEGSR
164700040119     C*
164800040119     C* 1° invio FIVAT
164900040119     C                   reset                   dscmz
165000040119     C                   move      vlrpoi        cmzdst
165100040119     C                   eval      cmzfld = 'FIVATWWR'
165200040119     C                   eval      cmzmbd = vlrhdl
165300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165400040119     C***                if        prmfir = *blanks
165500040119     C                   eval      cmzfla = 'FIVAT00F'
165600040119     C                   eval      cmzmba = 'FIVAT00F'
165700040119     C***                else
165800040119     C***                eval      cmzfla = prmfir
165900040119     C***                eval      cmzmba = prmfir
166000040119     C***                endif
166100040119     C                   eval      cmznrr = *zeros
166200040119     C                   move      §ctrokvt      cmznrr
166300040119     C                   eval      cmzlba = vlrfl1
166400040119     C                   call(e)   'TIS711C'
166500040119     C                   parm                    dscmz
166600040119     C                   parm      *blanks       esito
166700040119     C                   if        %error
166800040119     C                             or cmzerr = '1'
166900040119     C                             or esito  = '1'
167000040119     C                   eval      wrkesito = '3'
167100040119     C                   else
167200040119     C*
167300040119     C* 2° invio FIVAB
167400040119     C                   reset                   dscmz
167500040119     C                   move      vlrpoi        cmzdst
167600040119     C                   eval      cmzfld = vlrfou
167700040119     C                   eval      cmzmbd = vlrhdl
167800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167900040119     C***                if        prmfir = *blanks
168000040119     C                   eval      cmzfla = 'FIVAB00F'
168100040119     C                   eval      cmzmba = 'FIVAB00F'
168200040119     C***                else
168300040119     C***                eval      cmzfla = prmfir
168400040119     C***                eval      cmzmba = prmfir
168500040119     C***                endif
168600040119     C                   eval      cmznrr = *zeros
168700040119     C                   move      §ctrokvb      cmznrr
168800040119     C                   eval      cmzlba = vlrfl1
168900040119     C                   call(e)   'TIS711C'
169000040119     C                   parm                    dscmz
169100040119     C                   parm      *blanks       esito
169200040119     C                   if        %error
169300040119     C                             or cmzerr = '1'
169400040119     C                             or esito  = '1'
169500040119     C                   eval      wrkesito = '3'
169600040119     C                   endif
169700040119     C                   endif
169800040119     C*
169900040119     C                   ENDSR
170000040119     C***
170100010601
170200010601
170300010601
170400010601
170500010601      /TITLE Invio dei dati al punto operativo.
170600010601     C     opeini        BEGSR
170700010601     C*
170800010601     C* Inizializzo flag e contatori operativi
170900010601     C                   movel     '0'           flgGiro           1
171000010601     C                   movel     '0'           flgMulti          1
171100010601     C                   movel     '1'           flgStato          1
171200010615     C                   movel     '0'           flgOk             1
171300010601     C                   z-add     *zeros        cntNonEl         10 0
171400010601     C                   z-add     *zeros        depfil            3 0
171500010601     C                   z-add     *zeros        invfil            3 0
171600010601     C*
171700010601     C                   ENDSR
171800010601     C***
171900070326
172000070326
172100070326
172200070326
172300070326     C     *pssr         BEGSR
172400070329     C*
172500070329     C                   if        %open(tivin00r)
172600070329     C                   close     tivin00r
172700070329     C                   endif
172800070329     C                   if        %open(fivabwwr)
172900070329     C                   close     fivabwwr
173000070329     C                   endif
173100070329     C                   if        %open(fivatwwr)
173200070329     C                   close     fivatwwr
173300070329     C                   endif
173400070326     C*
173500070326     C* Effettuo la chiamata al CLLE preposto
173600070326     C                   call(e)   'TITVVTC'
173700070326     C                   parm                    parccm
173800070326     C                   parm                    parmbr
173900070326     C                   parm      '2'           paropz
174000070326     C*
174100070326     C                   eval      wrkesito = '2'
174200070404     C*
174300070404     C                   seton                                        LR
174400070326     C*
174500070326     C                   ENDSR     '*CANCL'
174600070326     C***
174700100318
174800100318
174900100318
175000100318     C     normCSV       BEGSR
175100100318     C*
175200100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
175300100318     C* separatori campi
175400100318     C                   if        CharTXT <> *blanks
175500100318     C                   z-add     *zeros        wLenVINDTA        4 0
175600100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175700100318     C                   setoff                                       80
175800100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
175900100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
176000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
176100100318     C                   if        not *in80
176200100318     C                   seton                                        80
176300100318     C                   else
176400100318     C                   setoff                                       80
176500100318     C                   endif
176600100318     C                   endif
176700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
176800100318     C                             and *in80 = *on
176900100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
177000100318     C                   endif
177100100318     C                   enddo
177200100318     C                   endif
177300100318     C*
177400100318     C                   ENDSR
177500100318     C***
177600070326
177700100326
177800100326
177900100326     C*----------------------------------------------------*
178000100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
178100100326     C*----------------------------------------------------*
178200100326     C     REPNSP        BEGSR
178300100326     C*
178400100326     C* NSP => Stacco un numeratore da AZNUM
178500100326     C                   clear                   TRUL33DS
178600100326     C                   eval      I33OPE = *zeros
178700100326     C                   eval      I33CNU = 302
178800100326     C                   eval      I33NUM = 1
178900100326     C                   movel     TRUL33DS      KPJBU
179000100326     C                   call      'TRUL33R'
179100100326     C                   parm                    KPJBA
179200100326     C                   movel     KPJBU         TRUL33DS
179300100326     C                   if        O33ERR = *zeros
179400100326     C                   z-add     O33NRF        VABNSP
179500100326     C                   z-add     O33NRF        VATNSP
179600100326     C                   else
179700100326     C                   SETON                                        31
179800100326     C                   EVAL      vinmsg = %trimr(vinmsg)
179900100326     C                             + ' ' + 'VABNSP VATNSP'
180000100326     C                   endif
180100100326     C*
180200100326     C                   ENDSR
180300010330
180400100326
180500010330
180600000613     C     *inzsr        BEGSR
180700990910     C*
180800990910     C     *entry        plist
180900990920     C                   parm                    tivlrds
181000990921     C                   parm      wrkesito      esito
181100000724     C                   parm                    prmlit
181200000710     C                   parm                    prmfir
181300010330     C*
181400010330     C* CALCOLA LA DATA CORRENTE
181500100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181600100318     C                   eval      datcor = %dec(%date() : *ISO)
181700000613     C*
181800000613     C                   ENDSR
181900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182000130703W_VABRMN;X;X;X;VABCAS;X;X;X;X;W_VABTIC;X;X;X;X;X;X;X;X;X;X;X;X;VATNOT_B;VATNOT_I;
182100130703VABRSD;VABRD2;VABIND;VABLOD;VABCAD;X;X;X;X;X;X;X;W_VABNOT;
182200130528
182300111116
182400100318
