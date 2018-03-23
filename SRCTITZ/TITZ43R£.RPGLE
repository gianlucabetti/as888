000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120509      *
000300120509      * PARTICOLARITA':
000400121025      * Il rif.num. è la parte numerica del loro nr.ordine (dal 4° char compreso in poi)
000500121025      * Aggiunta la colonna relativa alle e-mail
000600120716      * Il cliente passa l'ultimo CHI SONO etichettato. Per cui i CHI SONO si sviluppano così:
000700120716      * - prendere i primi 11 char del CHI SONO finale
000800120716      * - attaccarci 3 char con lo sviluppo da 1 a nr.colli
000900120716      * - attacare in fondo il 15o byte da reperire sul CHI SONO finale.
001000120716      * Quando il contrassegno viene passato, il tipo diventa BM
001100130607      * Se nel attuale campo note compare al inizio la dicitura 'A1'
001200130607      * o   Valorizzare il campo VABTC1 = 'A'
001300130607      * o   Quindi viene elisa la dicitura 'A1' dalle note (VABNOT / VABNT2)
001400120509      *
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
004900120716     D Intestazioni    s               n   inz(*on)
005000100326     D*------------------
005100100326     D* DS REPERIMENTO NUMERATORE
005200100326     D*------------------
005300100326     D trul33ds      e ds                  inz
005400100326     D*------------------
005500100326     D* DS ARCHITETTURA
005600100326     D*------------------
005700100326     D kpjba         e ds                  inz
005800100326
005900030715     D*------------------
006000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006100030715     D*------------------
006200030715     D SkSplitFLD      S             10    DIM(100)
006300030715     D SkSplitCSV      S            256    DIM(100)
006400030715     D CharCSV         S              1
006500030715     D CharTXT         S              1
006600030715     D CharNUM         S              1
006700100318     D CharSOS         S              1
006800030715     D posDa           S              3  0 INZ(*zeros)
006900030715     D posA            S              3  0 INZ(*zeros)
007000030715     D i               s              3  0 INZ(1)
007100030715     D wGiro           s              1  0 INZ(*zeros)
007200070823     D wVATNOT_A       s                   LIKE(VATNOT)
007300060331     D wVATNOT_B       s                   LIKE(VATNOT)
007400060331     D wVATNOT_E       s                   LIKE(VATNOT)
007500110927     D wVATNOT_I       s                   LIKE(VATNOT)
007600121025     D wVATNOT_J       s                   LIKE(VATNOT)
007700121025     D wVATNOT_IJ      s             40
007800120411     D wNOT            s             70
007900130607     D w100            s            100
008000120411     D Data8A          s              8
008100120524     D PrefixTel       s                   like(VATNOT)
008200120524     D Telefono        s                   like(VATNOT)
008300030822     D*------------------
008400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008500030822     D*------------------
008600030822     D posDaDft        S              3  0 INZ(*zeros)
008700030822     D posADft         S              3  0 INZ(*zeros)
008800030822     D j               s              3  0 INZ(1)
008900030822     D wGiroDft        s              1  0 INZ(*zeros)
009000080418     D*------------------
009100080418     D* Costanti
009200080418     D*------------------
009300080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009400080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009500100318
009600100318     D*------------------
009700100318     D* LINKING A DEFINIZIONI ESTERNE
009800100318     D*------------------
009900100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010000100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010100100318
010200010330
010300010330
010400010330
010500000913     C                   reset                   rrnum
010600990921     C                   reset                   esito
010700990921     C                   reset                   wrkesito
010800010601     C*
010900010601     C                   exsr      opeini
011000010605     C                   exsr      rwvab
011100010601     C*
011200010601     C                   seton                                        lr
011300010601
011400010601
011500010601
011600010601     C*--------------------------------------------------------
011700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011800010601     C*--------------------------------------------------------
011900010601     C     PREELA        BEGSR
012000010601     C*
012100010601     C* SE OCCORRE SPEDIRE IN FILIALE
012200010601     C                   if        invfil <> *zeros and
012300010601     C                             flgGiro = '0'
012400010601     C*
012500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012600010601     C                   eval      flgGiro = '1'
012700010601     C*
012800010601     C                   endif
012900010601     C*
013000010601     C                   ENDSR
013100010601     C***
013200010601
013300010601
013400010601
013500010601
013600010601     C*--------------------------------------------------------
013700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013800010601     C*--------------------------------------------------------
013900010601     C     ENDELA        BEGSR
014000040119     C*
014100040119     C* Effettuo la chiamata al CLLE preposto
014200040119     C                   call(e)   'TITVVTC'
014300040510     C                   parm                    parccm
014400040119     C                   parm                    parmbr
014500040119     C                   parm      '2'           paropz
014600050201     C*
014700050201     C* Effettuo lancio TISI95 solo x chiusura
014800050201     C                   CLEAR                   TISI95DS
014900050201     C                   EVAL      I95TLA = 'C'
015000050201     C                   CALL      'TISI95R'
015100050201     C                   PARM                    TISI95DS
015200000616     C*
015300010601     C                   ENDSR
015400010601
015500010601
015600010330     C*--------------------------------------------------------
015700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015800010330     C*--------------------------------------------------------
015900010605     C     RWVAB         BEGSR
016000010330     C*
016100010330     C                   if        not %open(tivin00r)
016200010330     C                   open      tivin00r
016300010330     C                   endif
016400021025     C                   if        not %open(fivabwwr)
016500021025     C                   open      fivabwwr
016600010330     C                   endif
016700040119     C*
016800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016900040119     C                   exsr      prevat
017000040119     C*
017100040119     C                   if        chkcall = '0'
017200040119     C*
017300040119     C                   if        not %open(fivatwwr)
017400040119     C                   open      fivatwwr
017500040119     C                   endif
017600010330     C*
017700040119     C                   clear                   §CTROKVB          7 0
017800040119     C                   clear                   §CTROKVT          7 0
017900040119     C                   clear                   §CTRMO            7 0
018000040119     C                   clear                   §CTRNO            7 0
018100120410     C*
018200120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018300120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018400120410     C                   IF        Intestazioni = *off
018500120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018600120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018700120410     C                   exsr      inzvar
018800120410     C                   exsr      impvab
018900120410     C                   ENDIF
019000010330     C*
019100030822     C                   do        *HIVAL
019200010330     C*
019300030822     C                   read      tivin00r                               70
019400010618     C*
019500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019600010618     C                   if        vinflg = '1'
019700010618     C                   eval      flgOk = '1'
019800010618     C                   endif
019900040510     C*
020000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020100040510     C                   z-add     *zeros        wLenVINDTA        4 0
020200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020600040510     C                   z-add     1             wFlgVINDTA
020700040510     C                   leave
020800040510     C                   endif
020900040510     C                   enddo
021000010618     C*
021100040510     C                   if        vindta > *blanks AND
021200040510     C                             wFlgVINDTA = 1
021300010330     C                   add       1             rrnum
021400010330     C*
021500010601     C                   if        *in70 = *off and
021600010330     C                             (vinflg = *blanks
021700010330     C                              or vinflg = '0'
021800010330     C                              or vinflg = '2')
021900010330     C*
022000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022100010711     C                   if        vinflg = *blanks or vinflg = '0'
022200010711     C                   clear                   vinmsg
022300010711     C                   endif
022400010601     C*
022500070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022600070925     C                   eval      vindta  = ' ' + %trim(vindta)
022700070925     C*
022800080527     C                   exsr      inzvar
022900100318     C                   exsr      normCSV
023000010605     C                   exsr      impvab
023100010601     C*
023200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023300010605     C                   eval      depfil = VABLNP
023400010601     C                   exsr      repfil
023500010601     C                   if        depfil = invfil
023600021025     C                   if        vlrpoi = 999
023700030822     C                   move(P)   invfil        VABFGS
023800021025     C                   else
023900030822     C                   move(P)   vlrpoi        VABFGS
024000021025     C                   endif
024100010601     C*
024200010601     C                   exsr      PREELA
024300010601     C*
024400010604     C* Ebbene...
024500010604     C*
024600040119     C  N31              add       1             §CTROKVB
024700040119     C   32              add       1             §CTRMO
024800040119     C   31              add       1             §CTRNO
024900080418     C*
025000030822     C                   if        wGiro = 2
025100080513     C* Scrivo sempre il VAT
025200080513     C  N31              exsr      wrivat
025300110201     C                   exsr      chkvab
025400080513     C                   endif
025500010604     C*
025600010604     C                   if        *in31 = *off and
025700010604     C                             *in32 = *off
025800010604     C                   eval      vinflg = '1'
025900010604     C                   else
026000010604     C                   eval      vinflg = '2'
026100010604     C                   endif
026200010604     C                   endif
026300010601     C*
026400010604     C                   endif
026500010604     C*
026600010330     C                   else
026700010330     C                   eval      vinflg = '1'
026800010330     C                   endif
026900010601     C*
027000010601     C  N70              update    tivin000
027100010330     C*
027200030822     C  N70              enddo
027300040119     C*
027400080527     C* Verifico dati rimasti "in cannna"...
027500080527     C                   seton                                        65
027600110201     C                   exsr      chkvab
027700080527     C*
027800040119     C                   endif                                                  (endif - chkcall)
027900010601     C*
028000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028100010601     C                   if        cntNonEl = *zeros or
028200010601     C                             flgMulti = '0'
028300010330     C* Se non ci sono record con errori ...
028400010601     C                   if        §ctrno = 0 and
028500010604     C                             §ctrmo = 0 and
028600010601     C                             flgStato <> '2'
028700010330     C* ... restituisco esito OK.
028800010330     C                   eval      wrkesito = '0'
028900010330     C                   else
029000040119     C                   if        §ctrokvb > 0
029100010330     C                   eval      wrkesito = '1'
029200010330     C                   else
029300010615     C                   if        flgOk = '0'
029400010615     C                   eval      wrkesito = '2'
029500010615     C                   else
029600010615     C                   eval      wrkesito = '6'
029700010615     C                   endif
029800010330     C                   endif
029900010330     C                   endif
030000010601     C                   else
030100010601     C                   eval      wrkesito = '9'
030200010601     C                   endif
030300010330     C*
030400010330     C                   if        %open(tivin00r)
030500010330     C                   close     tivin00r
030600010330     C                   endif
030700021025     C                   if        %open(fivabwwr)
030800021025     C                   close     fivabwwr
030900010330     C                   endif
031000040512     C                   if        %open(fivatwwr)
031100040512     C                   close     fivatwwr
031200040512     C                   endif
031300010601     C*
031400010601     C                   if        vlrpoi <> 999
031500010601     C                   eval      invfil = vlrpoi
031600010601     C                   endif
031700010330     C*
031800040119     C                   if        §ctrokvb > 0
031900010601     C                             and invfil > *zeros
032000010330     C                   exsr      invio
032100010330     C                   endif
032200010601     C*
032300010618     C                   if        flgGiro = '1'
032400010601     C                   exsr      endela
032500010618     C                   endif
032600010330     C*
032700010330     C                   ENDSR
032800010330     C***
032900080527
033000080527
033100080527
033200080527     C*----------------------------------------------------*
033300080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033400080527     C*----------------------------------------------------*
033500120509     C     CHKVAB        BEGSR
033600110201     C*
033700110201     C* Se richiesti ignorare colli / peso / volume
033800110315 xxx C***                if        *in35
033900110315 xxx C***                z-add     *zeros        VABNCL
034000110315 xxx C***                z-add     *zeros        VABPKB
034100110315 xxx C***                z-add     *zeros        VABVLB
034200110315 xxx C***                endif
034300080527     C*
034400120509     C* Scrivo il VAB solo a rottura d spedizione
034500080527     C                   if        depspe = *zeros
034600080527     C  N31              eval      fivabds_sav = fivabds
034700080527     C                   eval      depspe = vabNSP
034800080527     C                   else
034900080527     C                   if        depspe = vabNSP AND *in65 = *off
035000100413     C   51              add       VABNCL        sav_VABNCL
035100100413     C   51              add       VABPKB        sav_VABPKB
035200100413     C   51              add       VABVLB        sav_VABVLB
035300100720     C   52              z-add     VABNCL        sav_VABNCL
035400100720     C   52              z-add     VABPKB        sav_VABPKB
035500100720     C   52              z-add     VABVLB        sav_VABVLB
035600100720     C   53              add       VABNCL        sav_VABNCL
035700100720     C   53              z-add     VABPKB        sav_VABPKB
035800100720     C   53              z-add     VABVLB        sav_VABVLB
035900080527     C                   else
036000080527     C  N31              eval      fivabds_ok = fivabds_sav
036100080527     C  N31              exsr      wrivab
036200080527     C  N31              eval      fivabds_sav = fivabds
036300080527     C                   eval      depspe = vabNSP
036400080527     C                   endif
036500080527     C                   endif
036600080527     C*
036700080527     C                   ENDSR
036800040119
036900040119
037000040119
037100040119     C*----------------------------------------------------*
037200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037300040119     C*----------------------------------------------------*
037400040510     C     WRIVAB        BEGSR
037500080513     C*
037600080513     C* Gestisco l'eventuale rottura x numero spedizione
037700120716     C                   if        VABNSP <> depspe or *in65 = *on
037800070301     C                   if        wVATNOT_E <> *blanks
037900120716     C                   eval      ok_VABCTM = '7Q'
038000070301     C                   endif
038100100413     C                   write     fivab000
038200080513     C                   endif
038300040119     C*
038400040119     C                   ENDSR
038500040119
038600040119
038700040119
038800040119     C*----------------------------------------------------*
038900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039000040119     C*----------------------------------------------------*
039100040510     C     WRIVAT        BEGSR
039200060512     C*
039300060512     C* Inizializzo i campi chiave
039400060512     C                   eval      VATFGS = VABFGS
039500060512     C                   eval      VATAAS = VABAAS
039600060512     C                   eval      VATCCM = VABCCM
039700070823     C                   eval      VATNRS = VABNRS
039800060512     C                   eval      VATNSP = VABNSP
039900060512     C                   eval      VATLNP = VABLNP
040000040119     C*
040100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040200040510     C                   if        VATTRC = *blanks
040300040510     C                   eval      VATTRC = 'E'
040400040510     C                   endif
040500110318     C                   if        VATNOT <> *blanks
040600040510     C                   write     FIVAT000
040700040119     C                   add       1             §CTROKVT
040800110318     C                   endif
040900060331     C*
041000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041100060331     C                   if        wVATNOT_A <> *blanks
041200060331     C                   eval      VATNOT = wVATNOT_A
041300060331     C                   eval      VATTRC = 'A'
041400060331     C                   write     FIVAT000
041500060331     C                   add       1             §CTROKVT
041600060331     C                   endif
041700060331     C*
041800060331     C                   if        wVATNOT_B <> *blanks
041900060331     C                   eval      VATNOT = wVATNOT_B
042000060331     C                   eval      VATTRC = 'B'
042100060331     C                   write     FIVAT000
042200060331     C                   add       1             §CTROKVT
042300060331     C                   endif
042400060331     C*
042500060331     C                   if        wVATNOT_E <> *blanks
042600120716     C***                eval      VATNOT = wVATNOT_E
042700060331     C                   eval      VATTRC = 'E'
042800120524     C*
042900120524     C* Sviluppati i "CHI SONO" in relazione al numero colli
043000120524     C*
043100120716     C     1             DO        VABNCL        wNumCollo         3 0
043200120716     C*** da analisi del POC il progressivo colli è max 3 char e deve contiene gli zeri non
043300120524     C*** significativi
043400120716     C                   EVAL      VATNOT = %subst(wVATNOT_E:1:11) +
043500120716     C                                %editc(wNumCollo:'X') +
043600120716     C                                %subst(wVATNOT_E:15:1)
043700120410     C                   write     FIVAT000
043800120410     C                   add       1             §CTROKVT
043900120524     C                   ENDDO
044000120524     C*
044100120320     C                   endif
044200120320     C*
044300121025     C* e-mail
044400110927     C                   if        wVATNOT_I <> *blanks
044500110927     C                   eval      VATNOT = wVATNOT_I
044600110927     C                   eval      VATTRC = 'I'
044700110927     C                   write     FIVAT000
044800110927     C                   add       1             §CTROKVT
044900110927     C                   endif
045000121025     C*
045100121025     C* e-mail 2
045200121025     C                   if        wVATNOT_J <> *blanks
045300121025     C                   eval      VATNOT = wVATNOT_J
045400121025     C                   eval      VATTRC = 'J'
045500121025     C                   write     FIVAT000
045600121025     C                   add       1             §CTROKVT
045700121025     C                   endif
045800040119     C*
045900040119     C                   ENDSR
046000040119
046100040119
046200040119
046300040119     C*----------------------------------------------------*
046400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046500040119     C*----------------------------------------------------*
046600040119     C     PREVAT        BEGSR
046700040119     C*
046800040119     C* Compongo il nome del membro da dare al FIVATWWR
046900040119     C                   eval      parmbr = vlrhdl
047000040119     C                   movel     'M'           parmbr
047100040510     C                   eval      parccm = vlrksc
047200040119     C                   eval      paropz = '1'
047300040119     C* Effettuo la chiamata al CLLE preposto
047400040119     C                   call(e)   'TITVVTC'
047500040119     C                   parm                    parccm
047600040119     C                   parm                    parmbr
047700040119     C                   parm                    paropz
047800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047900040119     C                   if        %error
048000040119     C                   movel     '1'           chkcall
048100040119     C                   else
048200040119     C                   movel     '0'           chkcall
048300040119     C                   endif
048400040119     C*
048500040119     C                   ENDSR
048600010601
048700010601
048800010601
048900010330     C*----------------------------------------------------*
049000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049100010330     C*----------------------------------------------------*
049200040119     C     INZVAR        BEGSR
049300080527     C*
049400080527     C                   setoff                                       65
049500010330     C*
049600030822     C* Inizializzo variabili di wrk
049700020204     C                   Z-ADD     *zeros        Num5_0            5 0
049800101018     C                   MOVEL     *blanks       wFlgCAS
049900060331     C                   MOVEL     *blanks       wVATNOT_A
050000060331     C                   MOVEL     *blanks       wVATNOT_B
050100060331     C                   MOVEL     *blanks       wVATNOT_E
050200110927     C                   MOVEL     *blanks       wVATNOT_I
050300121025     C                   MOVEL     *blanks       wVATNOT_J
050400121025     C                   MOVEL     *blanks       wVATNOT_IJ
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
064700120418     C                   Z-ADD     PiVal         VABPKB
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
066000030822     C                   Z-ADD     PiVal         VABVLB
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
095800130111     C* Stacco un progressivo x numero spedizione, in quanto NSP = RMN
095900130111 xxx C                   EXSR      repNSP
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
097800030715     C                   ENDIF
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
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on
114000030721     C                   Z-ADD     PiVal         Num5_0
114100030721     C                   MOVEL(p)  Num5_0        VABCAD
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      VABCAD = *zeros
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600030721     C                             + ' ' + 'VABCAD'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
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
116300120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
116400120418     C* per cui si arrotonda il valore passato agli etti
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiNum=*on
116900120418     C                   Z-ADD(H)  PiVal         VABPKB
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      VABPKB = *zeros
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABPKB'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** CAS
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiNum=*on
118200030721     C                   Z-ADD     PiVal         VABCAS
118300120716     C                   IF        VABCAS > 0
118400141028     C                   EVAL      VABVCA = 'EUR'
118500141028     C                   EVAL      VABTIC = 'TM'
118600120716     C                   EVAL      wFlgCAS = '1'
118700120716     C                   ENDIF
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000030721     C                   EVAL      VABCAS = *zeros
119100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119200030721     C                             + ' ' + 'VABCAS'
119300030721     C                   ENDIF
119400030721     C                   ENDIF
119500030721     C*** CCM
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800030721     C                   EXSR      CHKNUM
119900030721     C                   IF        PiInt=*on      AND
120000030721     C                             PiVal<=9999999 AND
120100030721     C                             PiVal>*zeros
120200030721     C                   Z-ADD     PiVal         VABCCM
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      VABCCM = *zeros
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700040510     C                             + ' ' + 'VABCCM'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** LNP
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiInt=*on    AND
121500030721     C                             PiVal<=999   AND
121600030721     C                             PiVal>*zeros
121700030721     C                   Z-ADD     PiVal         VABLNP
121800030721     C                   ELSE
121900040715     C                   SETON                                        32
122000030721     C                   EVAL      VABLNP = *zeros
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200040510     C                             + ' ' + 'VABLNP'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** NRS
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiInt=*on AND
123000030721     C                             PiVal<=99
123100030721     C                   Z-ADD     PiVal         VABNRS
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABNRS'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** LNA
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on    AND
124300030721     C                             PiVal<=999   AND
124400030721     C                             PiVal>*zeros
124500030721     C                   Z-ADD     PiVal         VABLNA
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      VABLNA = *zeros
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000030721     C                             + ' ' + 'VABLNA'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** CTR
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiInt=*on  AND
125800030721     C                             PiVal<=999
125900030721     C                   Z-ADD     PiVal         VABCTR
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABCTR'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** IAS
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiNum=*on
127100030721     C                   Z-ADD     PiVal         VABIAS
127200030721     C                   ELSE
127300030721     C                   SETON                                        32
127400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127500030721     C                             + ' ' + 'VABIAS'
127600030721     C                   ENDIF
127700030721     C                   ENDIF
127800030721     C*** VLB
127900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128000030721     C                   EVAL      PiStr=SkSplitCSV(i)
128100030721     C                   EXSR      CHKNUM
128200030721     C                   IF        PiNum=*on
128300030721     C                   Z-ADD     PiVal         VABVLB
128400030721     C                   ELSE
128500030721     C                   SETON                                        32
128600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128700030721     C                             + ' ' + 'VABVLB'
128800030721     C                   ENDIF
128900030721     C                   ENDIF
129000030721     C*** QFT
129100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129200030721     C                   EVAL      PiStr=SkSplitCSV(i)
129300030721     C                   EXSR      CHKNUM
129400030721     C                   IF        PiNum=*on
129500030721     C                   Z-ADD     PiVal         VABQFT
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129900030721     C                             + ' ' + 'VABQFT'
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200030721     C*** NCD
130300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130400030721     C                   EVAL      PiStr=SkSplitCSV(i)
130500030721     C                   EXSR      CHKNUM
130600030721     C                   IF        PiInt=*on      AND
130700030721     C                             PiVal<=9999999
130800030721     C                   Z-ADD     PiVal         VABNCD
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABNCD'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** NCA
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiInt=*on      AND
132000030721     C                             PiVal<=9999999
132100030721     C                   Z-ADD     PiVal         VABNCA
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABNCA'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800030721     C*** ZNC
132900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiInt=*on AND
133300030721     C                             PiVal<=99
133400030721     C                   Z-ADD     PiVal         VABZNC
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABZNC'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** DCR
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiInt=*on       AND
134600030721     C                             PiVal<=99999999
134700030721     C                   Z-ADD     PiVal         VABDCR
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135100030721     C                             + ' ' + 'VABDCR'
135200030721     C                   ENDIF
135300030721     C                   ENDIF
135400030721     C*** HCR
135500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135600030721     C                   EVAL      PiStr=SkSplitCSV(i)
135700030721     C                   EXSR      CHKNUM
135800030721     C                   IF        PiInt=*on   AND
135900030721     C                             PiVal<=9999
136000030721     C                   Z-ADD     PiVal         VABHCR
136100030721     C                   ELSE
136200030721     C                   SETON                                        32
136300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136400030721     C                             + ' ' + 'VABHCR'
136500030721     C                   ENDIF
136600030721     C                   ENDIF
136700030721     C*** VMD
136800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
136900030721     C                   EVAL      PiStr=SkSplitCSV(i)
137000030721     C                   EXSR      CHKNUM
137100030721     C                   IF        PiNum=*on
137200030721     C                   Z-ADD     PiVal         VABVMD
137300030721     C                   ELSE
137400030721     C                   SETON                                        32
137500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137600030721     C                             + ' ' + 'VABVMD'
137700030721     C                   ENDIF
137800030721     C                   ENDIF
137900030721     C*** ANT
138000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138100030721     C                   EVAL      PiStr=SkSplitCSV(i)
138200030721     C                   EXSR      CHKNUM
138300030721     C                   IF        PiInt=*on        AND
138400030721     C                             PiVal<=999999999
138500030721     C                   Z-ADD     PiVal         VABANT
138600030721     C                   ELSE
138700030721     C                   SETON                                        32
138800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138900030721     C                             + ' ' + 'VABANT'
139000030721     C                   ENDIF
139100030721     C                   ENDIF
139200030721     C*** CMO
139300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139400030721     C                   EVAL      PiStr=SkSplitCSV(i)
139500030721     C                   EXSR      CHKNUM
139600030721     C                   IF        PiInt=*on
139700030721     C                   Z-ADD     PiVal         Num5_0
139800030721     C                   MOVEL(p)  Num5_0        VABCMO
139900030721     C                   ELSE
140000030721     C                   SETON                                        32
140100030721     C                   EVAL      VABCMO = *zeros
140200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140300030721     C                             + ' ' + 'VABCMO'
140400030721     C                   ENDIF
140500030721     C                   ENDIF
140600040510     C*
140700040510     C* Reperisco i campi del FIVAT
140800040510     C*** VATNOT
140900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141100040510     C                   ENDIF
141200040510     C*** VATTRC
141300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141500040510     C                   ENDIF
141600060331     C*** VATNOT_A
141700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
141800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
141900060331     C                   ENDIF
142000060331     C*** VATNOT_B
142100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142300060331     C                   ENDIF
142400060331     C*** VATNOT_E
142500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142700060331     C                   ENDIF
142800110927     C*** VATNOT_I
142900110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
143000110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
143100110927     C                   ENDIF
143200070823     C*
143300100831     C***  ===>  Gestione campi "particolari" dentro ciclo
143400120716
143500120716     C*** W_NOTE
143600120716     C                   IF        %trim(SkSplitFLD(i)) = 'W_NOTE'
143700130607     C                   EVAL      w100 = %trim(SkSplitCSV(i))
143800130607     C                   IF        %subst(w100:1:2) = 'A1'
143900130607     C                   EVAL      VABTC1 = 'A'
144000130607     C                   EVAL      %subst(w100:1:2) = *blank
144100130607     C                   EVAL      w100 = %trim(w100)
144200130607     C                   ENDIF
144300130607     C                   EVAL      VABNOT = %subst(w100:1:35)
144400130607     C                   EVAL      VABNT2 = %subst(w100:36:35)
144500120716     C                   ENDIF
144600120716
144700120716     C*** W_VABRMN
144800120716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
144900130111     C                   EVAL      PiStr=SkSplitCSV(i)
145000120716     C                   EXSR      CHKNUM
145100120716     C                   IF        PiInt=*on
145200120716     C                   Z-ADD     PiVal         VABRMN
145300130111     C***                Z-ADD     PiVal         VABNSP
145400120716     C                   ELSE
145500120716     C                   SETON                                        32
145600120716     C                   EVAL      VABRMN = *zeros
145700120716     C                   EVAL      vinmsg = %trimr(vinmsg)
145800130111     C                             + ' ' + 'VABRMN'
145900130111     C***                          + ' ' + 'VABRMN VABNSP'
146000120716     C                   ENDIF
146100120716     C                   ENDIF
146200120716
146300120716     C*** W_VATNOT_E
146400120716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
146500120716     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146600120716     C                   ENDIF
146700120716
146800121025     C*** W_VATNOT_I
146900121025     C* al massimo 40 char
147000121025     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
147100121025     C                   EVAL      wVATNOT_IJ =
147200121025     C                                     %trim(SkSplitCSV(i))
147300121025     C                   ENDIF
147400121025
147500070925     C*
147600071029     C***  <===  -----------------------------------------
147700040510     C*
147800030715     C                   ENDIF
147900030715     C                   EVAL      i = i + 1
148000030715     C                   ENDDO
148100070925     C*
148200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
148300120411     C*
148400121025     C*
148500130219     C* se il c/assegno non è valorizzato, non va spedita l'email
148600130219     C                   IF        wFlgCAS <> '1'
148700130219     C                   EVAL      wVATNOT_IJ = *blanks
148800130219     C                   ENDIF
148900121025     C                   IF        %trim(wVATNOT_IJ) <> *blanks
149000121025     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
149100121025     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:5)
149200121025     C                   ENDIF
149300120509
149400070925     C*
149500071029     C***  <===  -----------------------------------------
149600070925     C*
149700070925     C* Considerazioni finali su CBO/CAS
149800100318     C                   IF        wFlgCAS = '1'    OR
149900100318     C                             VABCAS  > *zeros
150000070925     C                   IF        VABCBO = '1'
150100070925     C                   EVAL      VABCBO = '4'
150200070925     C                   ENDIF
150300070925     C                   IF        VABCBO = '2'
150400070925     C                   EVAL      VABCBO = '6'
150500070925     C                   ENDIF
150600070925     C                   ENDIF
150700070925     C***  <===  ----------------------------
150800040510     C*
150900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
151000040510     C                   IF        VABNSP = *zeros
151100040510     C                   SETON                                        31
151200040510     C                   ENDIF
151300040715     C*
151400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
151500040715     C                   IF        VABLNP = *zeros
151600040715     C                   SETON                                        31
151700040715     C                   ENDIF
151800040510     C*
151900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
152000120411     C                   IF        VABRMN = *zeros
152100120411     C                   Z-ADD     VABNSP        VABRMN
152200120411     C                   ENDIF
152300120328     C* IN QUESTO CASO LO FORZO = 1
152400120411     C***                IF        VABRMN = *zeros
152500120411     C***                Z-ADD     1             VABRMN
152600120411     C***                ENDIF
152700040510     C*
152800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
152900040510     C                   IF        VABRMA = *blanks
153000040714     C                   MOVEL     VABRMN        VABRMA
153100040510     C                   ENDIF
153200040128     C*
153300040510     C* Se provincia nn valorizzata la reperisco
153400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
153500110912     C                   IF        VABPRD =  *blanks AND
153600110912     C                             VABNZD =  *blanks AND
153700110912     C                             VABCAD <> *blanks AND
153800110912     C                             VABLOD <> *blanks
153900040128     C                   CLEAR                   TISI95DS
154000040128     C                   EVAL      I95TCN = '3'
154100040128     C                   Z-ADD     datcor        I95DAT
154200040128     C                   EVAL      I95NAR = VABNZD
154300040128     C                   EVAL      I95CAP = VABCAD
154400040128     C                   EVAL      I95LOC = VABLOD
154500040128     C                   CALL      'TISI95R'
154600040128     C                   PARM                    TISI95DS
154700040128     C                   EVAL      VABPRD = O95PRV
154800040510     C                   ENDIF
154900020204     C*
155000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
155100020204     C                   EXSR      CHKIMPDIV
155200030715     C                   ENDIF
155300020204     C*
155400010607     C                   ENDSR
155500010607     C*----------------------------------------------------*
155600010601
155700020204
155800020204     C*----------------------------------------------------*
155900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
156000020204     C*----------------------------------------------------*
156100020204     C     CHKIMPDIV     BEGSR
156200020204     C*
156300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
156400020204     C                   Z-ADD     *zeros        wrkDec            9 9
156500020204     C*
156600020204     C* Come prima cosa effettuo considerazioni sulla divisa
156700020204     C                   IF        vabIAS > *zeros
156800020204     C                   IF        vabVAS <> 'EUR'
156900020204     C                   EVAL      vabVAS =  'ITL'
157000020204     C                   ENDIF
157100020204     C                   ENDIF
157200020204     C*
157300020204     C                   IF        vabCAS > *zeros
157400020204     C                   IF        vabVCA <> 'EUR'
157500020204     C                   EVAL      vabVCA =  'ITL'
157600020204     C                   ENDIF
157700020204     C                   ENDIF
157800020204     C*
157900020204     C                   IF        vabVMD > *zeros
158000020204     C                   IF        vabVAD <> 'EUR'
158100020204     C                   EVAL      vabVAD =  'ITL'
158200020204     C                   ENDIF
158300020204     C                   ENDIF
158400020204     C*
158500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
158600020204     C                   Z-ADD     vabIAS        wrkDec
158700020204     C                   IF        wrkDec > *zeros
158800020204     C                   IF        vabVAS = 'ITL'
158900020204     C                   EVAL      vabIAS = *zeros
159000020204     C                   ENDIF
159100020204     C                   ENDIF
159200020204     C*
159300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
159400020204     C                   Z-ADD     vabCAS        wrkDec
159500020204     C                   IF        wrkDec > *zeros
159600020204     C                   IF        vabVCA = 'ITL'
159700020204     C                   EVAL      vabCAS = *zeros
159800020204     C                   ENDIF
159900020204     C                   ENDIF
160000020204     C*
160100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
160200020204     C                   Z-ADD     vabVMD        wrkDec
160300020204     C                   IF        wrkDec > *zeros
160400020204     C                   IF        vabVAD = 'ITL'
160500020204     C                   EVAL      vabVMD = *zeros
160600020204     C                   ENDIF
160700020204     C                   ENDIF
160800020204     C*
160900020204     C                   ENDSR
161000020204     C***
161100010330
161200100318
161300100318
161400100318     C*----------------------------------------------------*
161500100318     C*  CONTROLLO NUMERICITA' CAMPI
161600100318     C*----------------------------------------------------*
161700100318     C     CHKNUM        BEGSR
161800100318     C*
161900100318     C                   IF        PiDecChr = *blanks
162000100318     C                   EVAL      PiDecChr = CharNUM
162100100318     C                   ENDIF
162200100318     C*
162300100318     C                   callp     UBISNUM_Check(PiStr
162400100318     C                                          :PiDecChr
162500100318     C                                          :PiVal
162600100318     C                                          :PiNum
162700100318     C                                          :PiInt)
162800100318     C*
162900100318     C                   ENDSR
163000100318     C***
163100010601
163200010601
163300010601
163400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
163500010601     C     repfil        BEGSR
163600010601     C*
163700010601     C                   if        invfil = *zeros and
163800010601     C                             depfil > *zeros and
163900010629     C                             (vinflg = *blanks or
164000010629     C                              vinflg = *zeros)
164100010601     C
164200010601     C                   eval      invfil = depfil
164300010601     C                   endif
164400010601     C*
164500010601     C                   if        depfil <> invfil and
164600010601     C                             invfil > *zeros
164700010601     C                   eval      flgMulti = '1'
164800010601     C                   if        vinflg = *blanks
164900010601     C                   add       1             cntNonEl
165000010601     C                   endif
165100010601     C                   endif
165200010601     C*
165300010601     C                   if        vinflg = '2'
165400010601     C                   eval      flgStato = '2'
165500010601     C                   endif
165600010601     C*
165700010601     C                   ENDSR
165800010601     C***
165900010601
166000010601
166100010601
166200010330
166300040119      /TITLE Invio dei dati al punto operativo.
166400040119     C     invio         BEGSR
166500040119     C*
166600040119     C* 1° invio FIVAT
166700040119     C                   reset                   dscmz
166800040119     C                   move      vlrpoi        cmzdst
166900040119     C                   eval      cmzfld = 'FIVATWWR'
167000040119     C                   eval      cmzmbd = vlrhdl
167100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167200040119     C***                if        prmfir = *blanks
167300040119     C                   eval      cmzfla = 'FIVAT00F'
167400040119     C                   eval      cmzmba = 'FIVAT00F'
167500040119     C***                else
167600040119     C***                eval      cmzfla = prmfir
167700040119     C***                eval      cmzmba = prmfir
167800040119     C***                endif
167900040119     C                   eval      cmznrr = *zeros
168000040119     C                   move      §ctrokvt      cmznrr
168100040119     C                   eval      cmzlba = vlrfl1
168200040119     C                   call(e)   'TIS711C'
168300040119     C                   parm                    dscmz
168400040119     C                   parm      *blanks       esito
168500040119     C                   if        %error
168600040119     C                             or cmzerr = '1'
168700040119     C                             or esito  = '1'
168800040119     C                   eval      wrkesito = '3'
168900040119     C                   else
169000040119     C*
169100040119     C* 2° invio FIVAB
169200040119     C                   reset                   dscmz
169300040119     C                   move      vlrpoi        cmzdst
169400040119     C                   eval      cmzfld = vlrfou
169500040119     C                   eval      cmzmbd = vlrhdl
169600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169700040119     C***                if        prmfir = *blanks
169800040119     C                   eval      cmzfla = 'FIVAB00F'
169900040119     C                   eval      cmzmba = 'FIVAB00F'
170000040119     C***                else
170100040119     C***                eval      cmzfla = prmfir
170200040119     C***                eval      cmzmba = prmfir
170300040119     C***                endif
170400040119     C                   eval      cmznrr = *zeros
170500040119     C                   move      §ctrokvb      cmznrr
170600040119     C                   eval      cmzlba = vlrfl1
170700040119     C                   call(e)   'TIS711C'
170800040119     C                   parm                    dscmz
170900040119     C                   parm      *blanks       esito
171000040119     C                   if        %error
171100040119     C                             or cmzerr = '1'
171200040119     C                             or esito  = '1'
171300040119     C                   eval      wrkesito = '3'
171400040119     C                   endif
171500040119     C                   endif
171600040119     C*
171700040119     C                   ENDSR
171800040119     C***
171900010601
172000010601
172100010601
172200010601
172300010601      /TITLE Invio dei dati al punto operativo.
172400010601     C     opeini        BEGSR
172500010601     C*
172600010601     C* Inizializzo flag e contatori operativi
172700010601     C                   movel     '0'           flgGiro           1
172800010601     C                   movel     '0'           flgMulti          1
172900010601     C                   movel     '1'           flgStato          1
173000010615     C                   movel     '0'           flgOk             1
173100010601     C                   z-add     *zeros        cntNonEl         10 0
173200010601     C                   z-add     *zeros        depfil            3 0
173300010601     C                   z-add     *zeros        invfil            3 0
173400010601     C*
173500010601     C                   ENDSR
173600010601     C***
173700070326
173800070326
173900070326
174000070326
174100070326     C     *pssr         BEGSR
174200070329     C*
174300070329     C                   if        %open(tivin00r)
174400070329     C                   close     tivin00r
174500070329     C                   endif
174600070329     C                   if        %open(fivabwwr)
174700070329     C                   close     fivabwwr
174800070329     C                   endif
174900070329     C                   if        %open(fivatwwr)
175000070329     C                   close     fivatwwr
175100070329     C                   endif
175200070326     C*
175300070326     C* Effettuo la chiamata al CLLE preposto
175400070326     C                   call(e)   'TITVVTC'
175500070326     C                   parm                    parccm
175600070326     C                   parm                    parmbr
175700070326     C                   parm      '2'           paropz
175800070326     C*
175900070326     C                   eval      wrkesito = '2'
176000070404     C*
176100070404     C                   seton                                        LR
176200070326     C*
176300070326     C                   ENDSR     '*CANCL'
176400070326     C***
176500100318
176600100318
176700100318
176800100318     C     normCSV       BEGSR
176900100318     C*
177000100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
177100100318     C* separatori campi
177200100318     C                   if        CharTXT <> *blanks
177300100318     C                   z-add     *zeros        wLenVINDTA        4 0
177400100318     C                   z-add     *zeros        wFlgVINDTA        1 0
177500100318     C                   setoff                                       80
177600100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
177700100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
177800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
177900100318     C                   if        not *in80
178000100318     C                   seton                                        80
178100100318     C                   else
178200100318     C                   setoff                                       80
178300100318     C                   endif
178400100318     C                   endif
178500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
178600100318     C                             and *in80 = *on
178700100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
178800100318     C                   endif
178900100318     C                   enddo
179000100318     C                   endif
179100100318     C*
179200100318     C                   ENDSR
179300100318     C***
179400070326
179500100326
179600100326
179700100326     C*----------------------------------------------------*
179800100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
179900100326     C*----------------------------------------------------*
180000100326     C     REPNSP        BEGSR
180100100326     C*
180200100326     C* NSP => Stacco un numeratore da AZNUM
180300100326     C                   clear                   TRUL33DS
180400100326     C                   eval      I33OPE = *zeros
180500100326     C                   eval      I33CNU = 302
180600100326     C                   eval      I33NUM = 1
180700100326     C                   movel     TRUL33DS      KPJBU
180800100326     C                   call      'TRUL33R'
180900100326     C                   parm                    KPJBA
181000100326     C                   movel     KPJBU         TRUL33DS
181100100326     C                   if        O33ERR = *zeros
181200100326     C                   z-add     O33NRF        VABNSP
181300100326     C                   z-add     O33NRF        VATNSP
181400100326     C                   else
181500100326     C                   SETON                                        31
181600100326     C                   EVAL      vinmsg = %trimr(vinmsg)
181700120716     C                             + ' ' + 'VABNSP VATNSP'
181800100326     C                   endif
181900100326     C*
182000100326     C                   ENDSR
182100010330
182200100326
182300010330
182400000613     C     *inzsr        BEGSR
182500990910     C*
182600990910     C     *entry        plist
182700990920     C                   parm                    tivlrds
182800990921     C                   parm      wrkesito      esito
182900000724     C                   parm                    prmlit
183000000710     C                   parm                    prmfir
183100010330     C*
183200010330     C* CALCOLA LA DATA CORRENTE
183300100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
183400100318     C                   eval      datcor = %dec(%date() : *ISO)
183500000613     C*
183600000613     C                   ENDSR
183700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
183800130111W_VABRMN;*;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;VABPKB;VABNCL;VABCAS;W_NOTE;*;VABRMA;
183900121025W_VATNOT_I;W_VATNOT_E
184000120418
184100111116
184200100318
