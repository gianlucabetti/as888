000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400140220      *
000500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600130917     H decedit('0,')
000700120209     F*
000800990910     Ftivin00r  uF   E             DISK    usropn
000900100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001000040119     FFIVATwwr  O    E             DISK    usropn
001100000313     D*
001200040113     D*------------
001300040113     D* SCHIEDE A PROGRAMMA
001400040113     D*------------
001500080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300040128     D tisi95ds      e ds
002400100413     D fivabds       e ds                  extname(fivab00f)
002500100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002600100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
002700010330     D esito           s              1
002800010330     D prmlit          s             10
002900010330     D prmfir          s             10
003000010330     D wrkesito        s                   like(esito)
003100010330     D rrnum           s              6  0 INZ(*zeros)
003200040510     D parccm          s              8    INZ(*blanks)
003300040119     D parmbr          s             10    INZ(*blanks)
003400040119     D paropz          s              1    INZ(*blanks)
003500040119     D chkcall         s              1    INZ(*blanks)
003600080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003700080418     D wFlgCAS         s              1    INZ(*blanks)
003800120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
003900120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004000140407     D Intestazioni    s               n   inz(*on)
004100100326     D*------------------
004200100326     D* DS REPERIMENTO NUMERATORE
004300100326     D*------------------
004400100326     D trul33ds      e ds                  inz
004500100326     D*------------------
004600100326     D* DS ARCHITETTURA
004700100326     D*------------------
004800100326     D kpjba         e ds                  inz
004900130611     D*------------------
005000130611     D TRUL28DSX     e ds                  inz
005100100326
005200030715     D*------------------
005300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005400030715     D*------------------
005500030715     D SkSplitFLD      S             10    DIM(100)
005600030715     D SkSplitCSV      S            256    DIM(100)
005700030715     D CharCSV         S              1
005800030715     D CharTXT         S              1
005900030715     D CharNUM         S              1
006000100318     D CharSOS         S              1
006100130626     D posDa           S              4  0 INZ(*zeros)
006200130626     D posA            S              4  0 INZ(*zeros)
006300030715     D i               s              3  0 INZ(1)
006400030715     D wGiro           s              1  0 INZ(*zeros)
006500070823     D wVATNOT_A       s                   LIKE(VATNOT)
006600060331     D wVATNOT_B       s                   LIKE(VATNOT)
006700060331     D wVATNOT_E       s                   LIKE(VATNOT)
006800110927     D wVATNOT_I       s                   LIKE(VATNOT)
006900140220     D wVATNOT_J       s                   LIKE(VATNOT)
007000140220     D wVATNOT_IJ      s             70
007100140319     D wVATNOT_S       s                   LIKE(VATNOT)
007200120411     D wNOT            s             70
007300120411     D Data8A          s              8
007400140527     D wBarcodeN       s             11s 0
007500140527     D wX              s              3s 0
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
021700130917     C*
021800130917     C* Aggiungo un byte a blanks all'inizio d ogni record
021900130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
022000130917     C* evitare che vengano elisi i blank
022100140220     C***                eval      vindta  = '*' + %trim(vindta)
022200130917     C*
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
042100060331     C                   eval      VATTRC = 'E'
042200140527     C*
042300140527     C* Sviluppati i "CHI SONO" in relazione al numero colli
042400140527     C* siccome il barcode ha una parte alfanumerica (i primi 2 char), sviluppo la parte numerica
042500140527     C* poi ci attacco davanti quella alfa
042600140527     C*
042700140527     C                   FOR       wX = 0 by 1 to VABNCL - 1
042800140527     C                   EVAL      wBarcodeN= %dec(%trim(%subst(wVATNOT_E:3:11))
042900140527     C                              : 11 : 0) + wX
043000140527     C* recupero il check digit
043100140527     C                   clear                   TRUL28DSX
043200140527     C                   eval      I284STD = 'E10'
043300140527     C                   eval      I284COD =
043400140527     C                              %editc(WBarcodeN : 'X')
043500140527     C                   call      'TRUL28R4'
043600140527     C                   parm                    TRUL28DSX
043700140527     C* Se tutto ok scrivo il "chi sono" corrente
043800140527     C                   IF        O284err = *blanks
043900140527     C                   EVAL      VATNOT = %subst(wVATNOT_E:1:2) +
044000140527     C                              %editc(WBarcodeN : 'X')
044100140527     C                              + %trim(O284CkD)
044200140527     C                   write     FIVAT000
044300140527     C                   add       1             §CTROKVT
044400140527     C                   ENDIF
044500140527     C                   ENDFOR
044600140527     C*
044700120320     C                   endif
044800110927     C*
044900110927     C                   if        wVATNOT_I <> *blanks
045000110927     C                   eval      VATNOT = wVATNOT_I
045100110927     C                   eval      VATTRC = 'I'
045200110927     C                   write     FIVAT000
045300110927     C                   add       1             §CTROKVT
045400110927     C                   endif
045500140220     C*
045600140220     C                   if        wVATNOT_J <> *blanks
045700140220     C                   eval      VATNOT = wVATNOT_J
045800140220     C                   eval      VATTRC = 'J'
045900140220     C                   write     FIVAT000
046000140220     C                   add       1             §CTROKVT
046100140220     C                   endif
046200140319     C*
046300140319     C                   if        wVATNOT_S <> *blanks
046400140319     C                   eval      VATNOT = wVATNOT_S
046500140319     C* i 2 flag devono essere blank
046600140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
046700140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
046800140319     C                   eval      VATTRC = 'S'
046900140319     C                   write     FIVAT000
047000140319     C                   add       1             §CTROKVT
047100140319     C                   endif
047200040119     C*
047300040119     C                   ENDSR
047400040119
047500040119
047600040119
047700040119     C*----------------------------------------------------*
047800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047900040119     C*----------------------------------------------------*
048000040119     C     PREVAT        BEGSR
048100040119     C*
048200040119     C* Compongo il nome del membro da dare al FIVATWWR
048300040119     C                   eval      parmbr = vlrhdl
048400040119     C                   movel     'M'           parmbr
048500040510     C                   eval      parccm = vlrksc
048600040119     C                   eval      paropz = '1'
048700040119     C* Effettuo la chiamata al CLLE preposto
048800040119     C                   call(e)   'TITVVTC'
048900040119     C                   parm                    parccm
049000040119     C                   parm                    parmbr
049100040119     C                   parm                    paropz
049200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049300040119     C                   if        %error
049400040119     C                   movel     '1'           chkcall
049500040119     C                   else
049600040119     C                   movel     '0'           chkcall
049700040119     C                   endif
049800040119     C*
049900040119     C                   ENDSR
050000010601
050100010601
050200010601
050300010330     C*----------------------------------------------------*
050400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050500010330     C*----------------------------------------------------*
050600040119     C     INZVAR        BEGSR
050700080527     C*
050800080527     C                   setoff                                       65
050900010330     C*
051000030822     C* Inizializzo variabili di wrk
051100020204     C                   Z-ADD     *zeros        Num5_0            5 0
051200101018     C                   MOVEL     *blanks       wFlgCAS
051300060331     C                   MOVEL     *blanks       wVATNOT_A
051400060331     C                   MOVEL     *blanks       wVATNOT_B
051500060331     C                   MOVEL     *blanks       wVATNOT_E
051600110927     C                   MOVEL     *blanks       wVATNOT_I
051700140220     C                   MOVEL     *blanks       wVATNOT_J
051800140220     C                   MOVEL     *blanks       wVATNOT_IJ
051900140319     C                   MOVEL     *blanks       wVATNOT_S
052000120411     C                   MOVEL     *blanks       wNOT
052100120411     C                   MOVEL     *blanks       Data8A
052200030822     C*
052300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052400030822     C                   CLEAR                   SkSplitCSV
052500030822     C                   EVAL      i = 1
052600030822     C                   EVAL      posDa = *zeros
052700030822     C                   EVAL      posA  = *zeros
052800080527     C*
052900080527     C                   CLEAR                   FIVABDS
053000080527     C                   CLEAR                   FIVAT000
053100030822     C*
053200030822     C* Reimposto i valori di default
053300030822     C                   EXSR      DEFCAM
053400030822     C*
053500010330     C                   ENDSR
053600010330     C*----------------------------------------------------*
053700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053800010330     C*----------------------------------------------------*
053900010330     C     DEFCAM        BEGSR
054000030822     C*
054100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054200030715     C* e delimitatore testo.
054300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054600100318     C*
054700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
054800100318     C                   EVAL      CharSOS = CharNUM
054900030822     C*
055000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055100100720     C                   SETOFF                                       505152
055200100720     C                   SETOFF                                       53
055300100413     C                   SELECT
055400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055500100720     C                   SETON                                        50
055600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
055700100413     C                   SETON                                        51
055800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
055900100720     C                   SETON                                        52
056000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
056100100720     C                   SETON                                        53
056200100413     C                   ENDSL
056300030822     C*
056400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056500030822     C                   EVAL      posDaDft = 1
056600030822     C                   EVAL      posADft  = 0
056700030822     C                   EVAL      wGiroDft = 0
056800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056900030822     C                             posDaDft > 0
057000030822     C*
057100030822     C* Gestisco il 1° giro
057200030822     C                   IF        wGiroDft = 0
057300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057500030822     C* Incremento il contatore dei "giri"
057600030822     C                   EVAL      wGiroDft = 1
057700030822     C                   ELSE
057800030822     C                   EVAL      posDaDft = posADft
057900030822     C                   ENDIF
058000030822     C* Eseguo lo scan x trovare la fine del campo corrente
058100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058200030822     C*
058300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058500030822     C                   IF        posDaDft > 0 AND
058600030822     C                             posADft  > 0
058700030822     C* NCL
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'NCL'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiInt=*on
059700030822     C                   Z-ADD     PiVal         VABNCL
059800030822     C                   ENDIF
059900030822     C                   ENDIF
060000030822     C* CCM
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'CCM'
060500030822     C                   EVAL      PiStr=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   EXSR      CHKNUM
060900030822     C                   IF        PiInt=*on
061000030822     C                   Z-ADD     PiVal         VABCCM
061100030822     C                   ENDIF
061200030822     C                   ENDIF
061300030822     C* LNP
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'LNP'
061800030822     C                   EVAL      PiStr=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   EXSR      CHKNUM
062200030822     C                   IF        PiInt=*on
062300030822     C                   Z-ADD     PiVal         VABLNP
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* NRS
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'NRS'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiInt=*on
063600030822     C                   Z-ADD     PiVal         VABNRS
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* CTR
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'CTR'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiInt=*on
064900030822     C                   Z-ADD     PiVal         VABCTR
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200030822     C* PKB
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'PKB'
065700030822     C                   EVAL      PiStr=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   EXSR      CHKNUM
066100030822     C                   IF        PiNum=*on
066200130610     C                   Z-ADD     PiVal         VABPKB
066300030822     C                   ENDIF
066400030822     C                   ENDIF
066500030822     C* VLB
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'VLB'
067000030822     C                   EVAL      PiStr=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   EXSR      CHKNUM
067400030822     C                   IF        PiNum=*on
067500130610     C                   Z-ADD     PiVal         VABVLB
067600030822     C                   ENDIF
067700030822     C                   ENDIF
067800030822     C* QFT
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'QFT'
068300030822     C                   EVAL      PiStr=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   EXSR      CHKNUM
068700030822     C                   IF        PiNum=*on
068800030822     C                   Z-ADD     PiVal         VABQFT
068900030822     C                   ENDIF
069000030822     C                   ENDIF
069100030822     C* CBO
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'CBO'
069600030822     C                   EVAL      VABCBO=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* TSP
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'TSP'
070500030822     C                   EVAL      VABTSP=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* VAS
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'VAS'
071400030822     C                   EVAL      VABVAS=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* VCA
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'VCA'
072300030822     C                   EVAL      VABVCA=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700070925     C* TIC
072800070925     C                   IF        %subst(
072900070925     C                             %subst(vlrppt:posDaDft+1:
073000070925     C                             posADft-posDaDft-1):1:3)
073100070925     C                             = 'TIC'
073200070925     C                   EVAL      VABTIC=%trim(%subst(
073300070925     C                             %subst(vlrppt:posDaDft+1:
073400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500070925     C                   ENDIF
073600030822     C* GCA
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'GCA'
074100030822     C                   EVAL      VABGCA=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* CTM
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'CTM'
075000030822     C                   EVAL      VABCTM=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* FFD
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'FFD'
075900030822     C                   EVAL      VABFFD=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* VAD
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'VAD'
076800030822     C                   EVAL      VABVAD=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* GMA
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'GMA'
077700030822     C                   EVAL      VABGMA=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* GGA
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'GGA'
078600030822     C                   EVAL      VABGGA=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* GVA
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'GVA'
079500030822     C                   EVAL      VABGVA=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* TC1
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'TC1'
080400030822     C                   EVAL      VABTC1=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* TC2
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'TC2'
081300030822     C                   EVAL      VABTC2=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700040714     C* VATTRC
081800040714     C                   IF        %subst(
081900040714     C                             %subst(vlrppt:posDaDft+1:
082000040714     C                             posADft-posDaDft-1):1:3)
082100040714     C                             = 'TRC'
082200040714     C                   EVAL      VATTRC=%trim(%subst(
082300040714     C                             %subst(vlrppt:posDaDft+1:
082400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500040714     C                   ENDIF
082600030822     C* ...
082700030822     C                   ENDIF
082800030822     C                   ENDDO
082900020204     C*
083000010330     C                   ENDSR
083100010607     C*----------------------------------------------------*
083200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083300010607     C*----------------------------------------------------*
083400010607     C     IMPVAB        BEGSR
083500010607     C*
083600010607     C                   SETOFF                                       3132
083700110315 xxx C***                SETON                                        35
083800030515     C*
083900010607     C                   MOVEL     datcor        VABAAS
084000010607     C                   MOVE      datcor        VABMGS
084100010607     C*
084200030715     C**********
084300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084400030715     C**********
084500030715     C                   IF        wGiro = *zeros
084600040113     C*
084700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085000080429     C                             %trim(intcol(5))<>*blanks
085100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085300080429     C                                      %trim(intcol(5))
085400070823     C     ';':CharCSV   XLATE     vindta        vindta
085500040510     C                   ENDIF
085600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085700031201     C     minu:maiu     XLATE     vindta        vindta
085800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085900030715     C                   DOW       posDa <= %len(%trim(vindta))
086000030715     C*
086100030715     C* Gestisco il 1° campo
086200030715     C                   IF        i = 1
086300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086400030715     C                   EVAL      posDa = 1
086500030715     C* Eseguo lo scan x trovare la fine del primo campo
086600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086700030715     C* A questo punto "estraggo" il campo corrente
086800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086900030715     C                                                    (posA-posDa))
087000030715     C* X i campi successivi al 1°
087100030715     C                   ELSE
087200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087300030715     C                   EVAL      posDa = posA + 1
087400030715     C* Eseguo lo scan x trovare la fine del campo corrente
087500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087600030715     C* Gestisco l'ultimo campo
087700030715     C                   IF        posA = *zeros
087800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087900030715     C* A questo punto "estraggo" il campo corrente
088000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088100030715     C                                                    (posA-posDa))
088200030715     C                   LEAVE
088300030715     C                   ELSE
088400030715     C* A questo punto "estraggo" il campo corrente
088500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088600030715     C                                                    (posA-posDa))
088700030715     C                   ENDIF
088800030715     C                   ENDIF
088900030715     C* Incremento il contatore d campo
089000030715     C                   EVAL      i = i +1
089100030715     C                   ENDDO
089200030715     C*
089300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089400030715     C* delimitazione testo
089500030715     C                   EVAL      i = 1
089600030715     C                   DOW       i < %elem(SkSplitFLD)
089700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089900030715     C                   EVAL      i = i + 1
090000030715     C                   ENDDO
090100030715     C                   EVAL      wGiro = 1
090200030715     C                   ELSE
090300030715     C                   EVAL      wGiro = 2
090400031204     C**********
090500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090600031204     C* d separatore campo
090700031204     C**********
090800031204     C                   EVAL      vindta = %trim(vindta)
090900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091000031204     C                   EVAL      vindta = %subst(vindta:2)
091100031204     C                   ENDDO
091200031204     C*
091300031204     C                   Z-ADD     *zeros        lunghInput        4 0
091400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091800110912     C                   ENDDO
091900030715     C**********
092000030715     C* Effettuo lo split del campo dati d input
092100030715     C**********
092200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092300030715     C                   DOW       posDa <= %len(%trim(vindta))
092400030715     C*
092500030715     C* Gestisco il 1° campo
092600030715     C                   IF        i = 1
092700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092800030715     C                   EVAL      posDa = 1
092900030715     C* Eseguo lo scan x trovare la fine del primo campo
093000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093100030715     C* A questo punto "estraggo" il campo corrente
093200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093300030715     C                                                    (posA-posDa))
093400030715     C* X i campi successivi al 1°
093500030715     C                   ELSE
093600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093700030715     C                   EVAL      posDa = posA + 1
093800031204     C* Verifico che nn vi sia il campo nn valorizzato
093900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094000031204     C* Se campo nn valorizzato skippo al prossimo
094100031204     C                   EVAL      SkSplitCSV(i) = *blanks
094200031204     C                   EVAL      posA  = posA + 1
094300031204     C                   ELSE
094400030715     C* Eseguo lo scan x trovare la fine del campo corrente
094500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094600030715     C* Gestisco l'ultimo campo
094700030715     C                   IF        posA = *zeros
094800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094900030715     C* A questo punto "estraggo" il campo corrente
095000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095100030715     C                                                    (posA-posDa))
095200030715     C                   LEAVE
095300030715     C                   ELSE
095400030715     C* A questo punto "estraggo" il campo corrente
095500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095600030715     C                                                    (posA-posDa))
095700030715     C                   ENDIF
095800031204     C                   ENDIF
095900030715     C                   ENDIF
096000030715     C* Incremento il contatore d campo
096100030715     C                   EVAL      i = i +1
096200030715     C                   ENDDO
096300030715     C*
096400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096500030715     C* delimitazione testo
096600030715     C                   EVAL      i = 1
096700030715     C                   DOW       i < %elem(SkSplitCSV)
096800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097000030715     C                   EVAL      i = i + 1
097100030715     C                   ENDDO
097200100720     C*
097300100720     C* Stacco un progressivo x numero spedizione
097400140527 xxx C***                EXSR      repNSP
097500100326     C*
097600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097700030715     C                   EVAL      i = 1
097800030715     C                   DOW       i < %elem(SkSplitFLD)
097900030715     C*
098000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098200030715     C                             SkSplitCSV(i) <> *blanks
098300030721     C*** ATB
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098500030721     C                   EVAL      VABATB = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030715     C*** RSD
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099200030715     C* ==
099300130528     C                   ENDIF
099400030721     C*** RD2
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030715     C*** IND
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100000030715     C                   EVAL      VABIND = SkSplitCSV(i)
100100030715     C                   ENDIF
100200030715     C*** LOD
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030715     C*** PRD
100700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100900030715     C                   ENDIF
101000030721     C*** NZD
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030715     C*** RMA
101500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101700030715     C                   ENDIF
101800030715     C*** NOT
101900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102100030715     C                   ENDIF
102200030715     C*** NT2
102300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102500030715     C                   ENDIF
102600030715     C*** VCA
102700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102900030715     C                   ENDIF
103000030715     C*** VAS
103100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103300030715     C                   ENDIF
103400030715     C*** FFD
103500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103700030715     C                   ENDIF
103800030721     C*** GC1
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** GC2
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** TSP
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104900030721     C                   ENDIF
105000040510     C*** CBO
105100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105300040510     C                   ENDIF
105400030721     C*** NAS
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** TIC
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** GCA
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** XCO
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** CTM
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** TCR
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** CTS
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** FTM
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** VAD
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** GMA
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** GGA
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** GVA
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*** TC1
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110500030721     C                   ENDIF
110600030721     C*** TC2
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110900030721     C                   ENDIF
111000030721     C*** SCL
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111300030721     C                   ENDIF
111400030721     C*** RMO
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111700030721     C                   ENDIF
111800030721     C*** NMO
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112100030721     C                   ENDIF
112200030721     C*
112300030721     C* Reperisco quindi i campi numerici...
112400040510     C*** NSP
112500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112600040510     C                   EVAL      PiStr=SkSplitCSV(i)
112700040510     C                   EXSR      CHKNUM
112800040510     C                   IF        PiInt=*on
112900040510     C                   Z-ADD     PiVal         VABNSP
113000040510     C                   ELSE
113100040510     C                   SETON                                        31
113200040510     C                   EVAL      VABNSP = *zeros
113300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113400040510     C                             + ' ' + 'VABNSP'
113500040510     C                   ENDIF
113600040510     C                   ENDIF
113700071106     C*** RMN
113800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113900071106     C                   EVAL      PiStr=SkSplitCSV(i)
114000071106     C                   EXSR      CHKNUM
114100071106     C                   IF        PiInt=*on
114200071106     C                   Z-ADD     PiVal         VABRMN
114300071106     C                   ELSE
114400071106     C                   SETON                                        32
114500071106     C                   EVAL      VABRMN = *zeros
114600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114700071106     C                             + ' ' + 'VABRMN'
114800071106     C                   ENDIF
114900071106     C                   ENDIF
115000030721     C*** CAD
115100130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115200130610     C                   EVAL      PiStr=SkSplitCSV(i)
115300130610     C                   EXSR      CHKNUM
115400130610     C                   IF        PiInt=*on
115500130610     C                   Z-ADD     PiVal         Num5_0
115600130610     C                   MOVEL(p)  Num5_0        VABCAD
115700130610     C                   ELSE
115800130610     C                   SETON                                        32
115900130610     C                   EVAL      VABCAD = *zeros
116000130610     C                   EVAL      vinmsg = %trimr(vinmsg)
116100130610     C                             + ' ' + 'VABCAD'
116200130610     C                   ENDIF
116300130605     C                   ENDIF
116400030721     C*** NCL
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiInt=*on
116900030721     C                   Z-ADD     PiVal         VABNCL
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      VABNCL = *zeros
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400040122     C                             + ' ' + 'VABNCL'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** PKB
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiNum=*on
118200130610     C                   Z-ADD(H)  PiVal         VABPKB
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      VABPKB = *zeros
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABPKB'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** CAS
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119200110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
119300111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
119400140513     C                             %trim(SkSplitCSV(i)) <> '0.00'  AND
119500140513     C                             %trim(SkSplitCSV(i)) <> '.00'
119600130703     C                   EVAL      VABVCA = 'EUR'
119700140513     C                   EVAL      wFlgCAS = '1'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiNum=*on
120100030721     C                   Z-ADD     PiVal         VABCAS
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      VABCAS = *zeros
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600030721     C                             + ' ' + 'VABCAS'
120700030721     C                   ENDIF
120800140513     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** CCM
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiInt=*on      AND
121500030721     C                             PiVal<=9999999 AND
121600030721     C                             PiVal>*zeros
121700030721     C                   Z-ADD     PiVal         VABCCM
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      VABCCM = *zeros
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200040510     C                             + ' ' + 'VABCCM'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** LNP
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiInt=*on    AND
123000030721     C                             PiVal<=999   AND
123100030721     C                             PiVal>*zeros
123200030721     C                   Z-ADD     PiVal         VABLNP
123300030721     C                   ELSE
123400040715     C                   SETON                                        32
123500030721     C                   EVAL      VABLNP = *zeros
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700040510     C                             + ' ' + 'VABLNP'
123800030721     C                   ENDIF
123900030721     C                   ENDIF
124000030721     C*** NRS
124100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124200030721     C                   EVAL      PiStr=SkSplitCSV(i)
124300030721     C                   EXSR      CHKNUM
124400030721     C                   IF        PiInt=*on AND
124500030721     C                             PiVal<=99
124600030721     C                   Z-ADD     PiVal         VABNRS
124700030721     C                   ELSE
124800030721     C                   SETON                                        32
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000030721     C                             + ' ' + 'VABNRS'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** LNA
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiInt=*on    AND
125800030721     C                             PiVal<=999   AND
125900030721     C                             PiVal>*zeros
126000030721     C                   Z-ADD     PiVal         VABLNA
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      VABLNA = *zeros
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABLNA'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** CTR
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiInt=*on  AND
127300030721     C                             PiVal<=999
127400030721     C                   Z-ADD     PiVal         VABCTR
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABCTR'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** IAS
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiNum=*on
128600030721     C                   Z-ADD     PiVal         VABIAS
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABIAS'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** VLB
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiNum=*on
129800130610     C                   Z-ADD(H)  PiVal         VABVLB
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABVLB'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** QFT
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiNum=*on
131000030721     C                   Z-ADD     PiVal         VABQFT
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABQFT'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** NCD
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiInt=*on      AND
132200030721     C                             PiVal<=9999999
132300030721     C                   Z-ADD     PiVal         VABNCD
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABNCD'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** NCA
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on      AND
133500030721     C                             PiVal<=9999999
133600030721     C                   Z-ADD     PiVal         VABNCA
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABNCA'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** ZNC
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on AND
134800030721     C                             PiVal<=99
134900030721     C                   Z-ADD     PiVal         VABZNC
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABZNC'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** DCR
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on       AND
136100030721     C                             PiVal<=99999999
136200030721     C                   Z-ADD     PiVal         VABDCR
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABDCR'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900030721     C*** HCR
137000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137100030721     C                   EVAL      PiStr=SkSplitCSV(i)
137200030721     C                   EXSR      CHKNUM
137300030721     C                   IF        PiInt=*on   AND
137400030721     C                             PiVal<=9999
137500030721     C                   Z-ADD     PiVal         VABHCR
137600030721     C                   ELSE
137700030721     C                   SETON                                        32
137800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137900030721     C                             + ' ' + 'VABHCR'
138000030721     C                   ENDIF
138100030721     C                   ENDIF
138200030721     C*** VMD
138300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
138400030721     C                   EVAL      PiStr=SkSplitCSV(i)
138500030721     C                   EXSR      CHKNUM
138600030721     C                   IF        PiNum=*on
138700030721     C                   Z-ADD     PiVal         VABVMD
138800030721     C                   ELSE
138900030721     C                   SETON                                        32
139000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139100030721     C                             + ' ' + 'VABVMD'
139200030721     C                   ENDIF
139300030721     C                   ENDIF
139400030721     C*** ANT
139500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
139600030721     C                   EVAL      PiStr=SkSplitCSV(i)
139700030721     C                   EXSR      CHKNUM
139800030721     C                   IF        PiInt=*on        AND
139900030721     C                             PiVal<=999999999
140000030721     C                   Z-ADD     PiVal         VABANT
140100030721     C                   ELSE
140200030721     C                   SETON                                        32
140300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140400030721     C                             + ' ' + 'VABANT'
140500030721     C                   ENDIF
140600030721     C                   ENDIF
140700030721     C*** CMO
140800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
140900030721     C                   EVAL      PiStr=SkSplitCSV(i)
141000030721     C                   EXSR      CHKNUM
141100030721     C                   IF        PiInt=*on
141200030721     C                   Z-ADD     PiVal         Num5_0
141300030721     C                   MOVEL(p)  Num5_0        VABCMO
141400030721     C                   ELSE
141500030721     C                   SETON                                        32
141600030721     C                   EVAL      VABCMO = *zeros
141700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141800030721     C                             + ' ' + 'VABCMO'
141900030721     C                   ENDIF
142000030721     C                   ENDIF
142100040510     C*
142200040510     C* Reperisco i campi del FIVAT
142300040510     C*** VATNOT
142400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
142500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
142600040510     C                   ENDIF
142700040510     C*** VATTRC
142800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
142900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
143000040510     C                   ENDIF
143100060331     C*** VATNOT_A
143200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
143300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
143400060331     C                   ENDIF
143500060331     C*** VATNOT_B
143600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
143700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
143800060331     C                   ENDIF
143900060331     C*** VATNOT_E
144000070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
144100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144200060331     C                   ENDIF
144300110927     C*** VATNOT_I
144400110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
144500110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
144600110927     C                   ENDIF
144700140319     C*** VATNOT_S
144800140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
144900140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
145000140319     C                   ENDIF
145100070823     C*
145200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
145300140513
145400140527     C*** W_VABNSP
145500140527     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
145600140527     C                   EVAL      PiStr=SkSplitCSV(i)
145700140527     C                   EXSR      CHKNUM
145800140527     C                   IF        PiInt=*on
145900140527     C                   Z-ADD     PiVal         VABNSP
146000140527     C                   Z-ADD     PiVal         VABRMN
146100140527     C                   ELSE
146200140527     C                   SETON                                        31
146300140527     C                   EVAL      VABNSP = *zeros
146400140527     C                   EVAL      VABRMN = *zeros
146500140527     C                   EVAL      vinmsg = %trimr(vinmsg)
146600140527     C                             + ' ' + 'VABNSP VABRMN'
146700140527     C                   ENDIF
146800140527     C                   ENDIF
146900140527
147000140513     C*** W_VATNOT_E
147100140513     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
147200140513     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
147300140513     C                   ENDIF
147400070925     C*
147500071029     C***  <===  -----------------------------------------
147600040510     C*
147700030715     C                   ENDIF
147800030715     C                   EVAL      i = i + 1
147900030715     C                   ENDDO
148000070925     C*
148100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
148200140319     C*
148300070925     C*
148400071029     C***  <===  -----------------------------------------
148500070925     C*
148600070925     C* Considerazioni finali su CBO/CAS
148700140220     C                   IF        wFlgCAS = '1'    OR
148800140220     C                             VABCAS  > *zeros
148900140220     C                   IF        VABCBO = '1'
149000140220     C                   EVAL      VABCBO = '4'
149100140220     C                   ENDIF
149200140220     C                   IF        VABCBO = '2'
149300140220     C                   EVAL      VABCBO = '6'
149400140220     C                   ENDIF
149500140220     C                   ENDIF
149600070925     C***  <===  ----------------------------
149700040510     C*
149800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149900040510     C                   IF        VABNSP = *zeros
150000040510     C                   SETON                                        31
150100040510     C                   ENDIF
150200040715     C*
150300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
150400040715     C                   IF        VABLNP = *zeros
150500040715     C                   SETON                                        31
150600040715     C                   ENDIF
150700040510     C*
150800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150900120411     C                   IF        VABRMN = *zeros
151000120411     C                   Z-ADD     VABNSP        VABRMN
151100120411     C                   ENDIF
151200120328     C* IN QUESTO CASO LO FORZO = 1
151300120411     C***                IF        VABRMN = *zeros
151400120411     C***                Z-ADD     1             VABRMN
151500120411     C***                ENDIF
151600040510     C*
151700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
151800040510     C                   IF        VABRMA = *blanks
151900040714     C                   MOVEL     VABRMN        VABRMA
152000040510     C                   ENDIF
152100040128     C*
152200040510     C* Se provincia nn valorizzata la reperisco
152300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
152400110912     C                   IF        VABPRD =  *blanks AND
152500110912     C                             VABNZD =  *blanks AND
152600110912     C                             VABCAD <> *blanks AND
152700110912     C                             VABLOD <> *blanks
152800040128     C                   CLEAR                   TISI95DS
152900040128     C                   EVAL      I95TCN = '3'
153000040128     C                   Z-ADD     datcor        I95DAT
153100040128     C                   EVAL      I95NAR = VABNZD
153200040128     C                   EVAL      I95CAP = VABCAD
153300040128     C                   EVAL      I95LOC = VABLOD
153400040128     C                   CALL      'TISI95R'
153500040128     C                   PARM                    TISI95DS
153600040128     C                   EVAL      VABPRD = O95PRV
153700040510     C                   ENDIF
153800020204     C*
153900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
154000020204     C                   EXSR      CHKIMPDIV
154100030715     C                   ENDIF
154200020204     C*
154300010607     C                   ENDSR
154400010607     C*----------------------------------------------------*
154500010601
154600020204
154700020204     C*----------------------------------------------------*
154800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154900020204     C*----------------------------------------------------*
155000020204     C     CHKIMPDIV     BEGSR
155100020204     C*
155200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
155300020204     C                   Z-ADD     *zeros        wrkDec            9 9
155400020204     C*
155500020204     C* Come prima cosa effettuo considerazioni sulla divisa
155600020204     C                   IF        vabIAS > *zeros
155700020204     C                   IF        vabVAS <> 'EUR'
155800020204     C                   EVAL      vabVAS =  'ITL'
155900020204     C                   ENDIF
156000020204     C                   ENDIF
156100020204     C*
156200020204     C                   IF        vabCAS > *zeros
156300020204     C                   IF        vabVCA <> 'EUR'
156400020204     C                   EVAL      vabVCA =  'ITL'
156500020204     C                   ENDIF
156600020204     C                   ENDIF
156700020204     C*
156800020204     C                   IF        vabVMD > *zeros
156900020204     C                   IF        vabVAD <> 'EUR'
157000020204     C                   EVAL      vabVAD =  'ITL'
157100020204     C                   ENDIF
157200020204     C                   ENDIF
157300020204     C*
157400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
157500020204     C                   Z-ADD     vabIAS        wrkDec
157600020204     C                   IF        wrkDec > *zeros
157700020204     C                   IF        vabVAS = 'ITL'
157800020204     C                   EVAL      vabIAS = *zeros
157900020204     C                   ENDIF
158000020204     C                   ENDIF
158100020204     C*
158200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
158300020204     C                   Z-ADD     vabCAS        wrkDec
158400020204     C                   IF        wrkDec > *zeros
158500020204     C                   IF        vabVCA = 'ITL'
158600020204     C                   EVAL      vabCAS = *zeros
158700020204     C                   ENDIF
158800020204     C                   ENDIF
158900020204     C*
159000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
159100020204     C                   Z-ADD     vabVMD        wrkDec
159200020204     C                   IF        wrkDec > *zeros
159300020204     C                   IF        vabVAD = 'ITL'
159400020204     C                   EVAL      vabVMD = *zeros
159500020204     C                   ENDIF
159600020204     C                   ENDIF
159700020204     C*
159800020204     C                   ENDSR
159900020204     C***
160000010330
160100100318
160200100318
160300100318     C*----------------------------------------------------*
160400100318     C*  CONTROLLO NUMERICITA' CAMPI
160500100318     C*----------------------------------------------------*
160600100318     C     CHKNUM        BEGSR
160700100318     C*
160800100318     C                   IF        PiDecChr = *blanks
160900100318     C                   EVAL      PiDecChr = CharNUM
161000100318     C                   ENDIF
161100100318     C*
161200100318     C                   callp     UBISNUM_Check(PiStr
161300100318     C                                          :PiDecChr
161400100318     C                                          :PiVal
161500100318     C                                          :PiNum
161600100318     C                                          :PiInt)
161700100318     C*
161800100318     C                   ENDSR
161900100318     C***
162000010601
162100010601
162200010601
162300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
162400010601     C     repfil        BEGSR
162500010601     C*
162600010601     C                   if        invfil = *zeros and
162700010601     C                             depfil > *zeros and
162800010629     C                             (vinflg = *blanks or
162900010629     C                              vinflg = *zeros)
163000010601     C
163100010601     C                   eval      invfil = depfil
163200010601     C                   endif
163300010601     C*
163400010601     C                   if        depfil <> invfil and
163500010601     C                             invfil > *zeros
163600010601     C                   eval      flgMulti = '1'
163700010601     C                   if        vinflg = *blanks
163800010601     C                   add       1             cntNonEl
163900010601     C                   endif
164000010601     C                   endif
164100010601     C*
164200010601     C                   if        vinflg = '2'
164300010601     C                   eval      flgStato = '2'
164400010601     C                   endif
164500010601     C*
164600010601     C                   ENDSR
164700010601     C***
164800010601
164900010601
165000010601
165100010330
165200040119      /TITLE Invio dei dati al punto operativo.
165300040119     C     invio         BEGSR
165400040119     C*
165500040119     C* 1° invio FIVAT
165600040119     C                   reset                   dscmz
165700040119     C                   move      vlrpoi        cmzdst
165800040119     C                   eval      cmzfld = 'FIVATWWR'
165900040119     C                   eval      cmzmbd = vlrhdl
166000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166100040119     C***                if        prmfir = *blanks
166200040119     C                   eval      cmzfla = 'FIVAT00F'
166300040119     C                   eval      cmzmba = 'FIVAT00F'
166400040119     C***                else
166500040119     C***                eval      cmzfla = prmfir
166600040119     C***                eval      cmzmba = prmfir
166700040119     C***                endif
166800040119     C                   eval      cmznrr = *zeros
166900040119     C                   move      §ctrokvt      cmznrr
167000040119     C                   eval      cmzlba = vlrfl1
167100040119     C                   call(e)   'TIS711C'
167200040119     C                   parm                    dscmz
167300040119     C                   parm      *blanks       esito
167400040119     C                   if        %error
167500040119     C                             or cmzerr = '1'
167600040119     C                             or esito  = '1'
167700040119     C                   eval      wrkesito = '3'
167800040119     C                   else
167900040119     C*
168000040119     C* 2° invio FIVAB
168100040119     C                   reset                   dscmz
168200040119     C                   move      vlrpoi        cmzdst
168300040119     C                   eval      cmzfld = vlrfou
168400040119     C                   eval      cmzmbd = vlrhdl
168500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168600040119     C***                if        prmfir = *blanks
168700040119     C                   eval      cmzfla = 'FIVAB00F'
168800040119     C                   eval      cmzmba = 'FIVAB00F'
168900040119     C***                else
169000040119     C***                eval      cmzfla = prmfir
169100040119     C***                eval      cmzmba = prmfir
169200040119     C***                endif
169300040119     C                   eval      cmznrr = *zeros
169400040119     C                   move      §ctrokvb      cmznrr
169500040119     C                   eval      cmzlba = vlrfl1
169600040119     C                   call(e)   'TIS711C'
169700040119     C                   parm                    dscmz
169800040119     C                   parm      *blanks       esito
169900040119     C                   if        %error
170000040119     C                             or cmzerr = '1'
170100040119     C                             or esito  = '1'
170200040119     C                   eval      wrkesito = '3'
170300040119     C                   endif
170400040119     C                   endif
170500040119     C*
170600040119     C                   ENDSR
170700040119     C***
170800010601
170900010601
171000010601
171100010601
171200010601      /TITLE Invio dei dati al punto operativo.
171300010601     C     opeini        BEGSR
171400010601     C*
171500010601     C* Inizializzo flag e contatori operativi
171600010601     C                   movel     '0'           flgGiro           1
171700010601     C                   movel     '0'           flgMulti          1
171800010601     C                   movel     '1'           flgStato          1
171900010615     C                   movel     '0'           flgOk             1
172000010601     C                   z-add     *zeros        cntNonEl         10 0
172100010601     C                   z-add     *zeros        depfil            3 0
172200010601     C                   z-add     *zeros        invfil            3 0
172300010601     C*
172400010601     C                   ENDSR
172500010601     C***
172600070326
172700070326
172800070326
172900070326
173000070326     C     *pssr         BEGSR
173100070329     C*
173200070329     C                   if        %open(tivin00r)
173300070329     C                   close     tivin00r
173400070329     C                   endif
173500070329     C                   if        %open(fivabwwr)
173600070329     C                   close     fivabwwr
173700070329     C                   endif
173800070329     C                   if        %open(fivatwwr)
173900070329     C                   close     fivatwwr
174000070329     C                   endif
174100070326     C*
174200070326     C* Effettuo la chiamata al CLLE preposto
174300070326     C                   call(e)   'TITVVTC'
174400070326     C                   parm                    parccm
174500070326     C                   parm                    parmbr
174600070326     C                   parm      '2'           paropz
174700070326     C*
174800070326     C                   eval      wrkesito = '2'
174900070404     C*
175000070404     C                   seton                                        LR
175100070326     C*
175200070326     C                   ENDSR     '*CANCL'
175300070326     C***
175400100318
175500100318
175600100318
175700100318     C     normCSV       BEGSR
175800100318     C*
175900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
176000100318     C* separatori campi
176100100318     C                   if        CharTXT <> *blanks
176200100318     C                   z-add     *zeros        wLenVINDTA        4 0
176300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
176400100318     C                   setoff                                       80
176500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
176600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
176700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
176800100318     C                   if        not *in80
176900100318     C                   seton                                        80
177000100318     C                   else
177100100318     C                   setoff                                       80
177200100318     C                   endif
177300100318     C                   endif
177400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
177500100318     C                             and *in80 = *on
177600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
177700100318     C                   endif
177800100318     C                   enddo
177900100318     C                   endif
178000100318     C*
178100100318     C                   ENDSR
178200100318     C***
178300070326
178400100326
178500100326
178600100326     C*----------------------------------------------------*
178700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
178800100326     C*----------------------------------------------------*
178900100326     C     REPNSP        BEGSR
179000100326     C*
179100100326     C* NSP => Stacco un numeratore da AZNUM
179200100326     C                   clear                   TRUL33DS
179300100326     C                   eval      I33OPE = *zeros
179400100326     C                   eval      I33CNU = 302
179500100326     C                   eval      I33NUM = 1
179600100326     C                   movel     TRUL33DS      KPJBU
179700100326     C                   call      'TRUL33R'
179800100326     C                   parm                    KPJBA
179900100326     C                   movel     KPJBU         TRUL33DS
180000100326     C                   if        O33ERR = *zeros
180100100326     C                   z-add     O33NRF        VABNSP
180200100326     C                   z-add     O33NRF        VATNSP
180300140305     C                   z-add     O33NRF        VABRMN
180400100326     C                   else
180500100326     C                   SETON                                        31
180600100326     C                   EVAL      vinmsg = %trimr(vinmsg)
180700140305     C                             + ' ' + 'VABNSP VATNSP VABRMN'
180800100326     C                   endif
180900100326     C*
181000100326     C                   ENDSR
181100010330
181200100326
181300010330
181400000613     C     *inzsr        BEGSR
181500990910     C*
181600990910     C     *entry        plist
181700990920     C                   parm                    tivlrds
181800990921     C                   parm      wrkesito      esito
181900000724     C                   parm                    prmlit
182000000710     C                   parm                    prmfir
182100010330     C*
182200010330     C* CALCOLA LA DATA CORRENTE
182300100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
182400100318     C                   eval      datcor = %dec(%date() : *ISO)
182500000613     C*
182600000613     C                   ENDSR
182700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182800140527W_VABNSP;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;X;W_VATNOT_E;X;
182900140513
183000140512
183100111116
183200100318
