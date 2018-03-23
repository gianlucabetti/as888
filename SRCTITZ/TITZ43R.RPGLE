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
007800150504     D wVATNOT_S       s                   LIKE(VATNOT)
007900120411     D wNOT            s             70
008000150504     D wS1             s              2
008100130607     D w100            s            100
008200120411     D Data8A          s              8
008300120524     D PrefixTel       s                   like(VATNOT)
008400120524     D Telefono        s                   like(VATNOT)
008500030822     D*------------------
008600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008700030822     D*------------------
008800030822     D posDaDft        S              3  0 INZ(*zeros)
008900030822     D posADft         S              3  0 INZ(*zeros)
009000030822     D j               s              3  0 INZ(1)
009100030822     D wGiroDft        s              1  0 INZ(*zeros)
009200080418     D*------------------
009300080418     D* Costanti
009400080418     D*------------------
009500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009700100318
009800100318     D*------------------
009900100318     D* LINKING A DEFINIZIONI ESTERNE
010000100318     D*------------------
010100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010300100318
010400010330
010500010330
010600010330
010700000913     C                   reset                   rrnum
010800990921     C                   reset                   esito
010900990921     C                   reset                   wrkesito
011000010601     C*
011100010601     C                   exsr      opeini
011200010605     C                   exsr      rwvab
011300010601     C*
011400010601     C                   seton                                        lr
011500010601
011600010601
011700010601
011800010601     C*--------------------------------------------------------
011900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012000010601     C*--------------------------------------------------------
012100010601     C     PREELA        BEGSR
012200010601     C*
012300010601     C* SE OCCORRE SPEDIRE IN FILIALE
012400010601     C                   if        invfil <> *zeros and
012500010601     C                             flgGiro = '0'
012600010601     C*
012700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012800010601     C                   eval      flgGiro = '1'
012900010601     C*
013000010601     C                   endif
013100010601     C*
013200010601     C                   ENDSR
013300010601     C***
013400010601
013500010601
013600010601
013700010601
013800010601     C*--------------------------------------------------------
013900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014000010601     C*--------------------------------------------------------
014100010601     C     ENDELA        BEGSR
014200040119     C*
014300040119     C* Effettuo la chiamata al CLLE preposto
014400040119     C                   call(e)   'TITVVTC'
014500040510     C                   parm                    parccm
014600040119     C                   parm                    parmbr
014700040119     C                   parm      '2'           paropz
014800050201     C*
014900050201     C* Effettuo lancio TISI95 solo x chiusura
015000050201     C                   CLEAR                   TISI95DS
015100050201     C                   EVAL      I95TLA = 'C'
015200050201     C                   CALL      'TISI95R'
015300050201     C                   PARM                    TISI95DS
015400000616     C*
015500010601     C                   ENDSR
015600010601
015700010601
015800010330     C*--------------------------------------------------------
015900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600021025     C                   if        not %open(fivabwwr)
016700021025     C                   open      fivabwwr
016800010330     C                   endif
016900040119     C*
017000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017100040119     C                   exsr      prevat
017200040119     C*
017300040119     C                   if        chkcall = '0'
017400040119     C*
017500040119     C                   if        not %open(fivatwwr)
017600040119     C                   open      fivatwwr
017700040119     C                   endif
017800010330     C*
017900040119     C                   clear                   §CTROKVB          7 0
018000040119     C                   clear                   §CTROKVT          7 0
018100040119     C                   clear                   §CTRMO            7 0
018200040119     C                   clear                   §CTRNO            7 0
018300120410     C*
018400120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018500120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018600120410     C                   IF        Intestazioni = *off
018700120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018800120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018900120410     C                   exsr      inzvar
019000120410     C                   exsr      impvab
019100120410     C                   ENDIF
019200010330     C*
019300030822     C                   do        *HIVAL
019400010330     C*
019500030822     C                   read      tivin00r                               70
019600010618     C*
019700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019800010618     C                   if        vinflg = '1'
019900010618     C                   eval      flgOk = '1'
020000010618     C                   endif
020100040510     C*
020200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020300040510     C                   z-add     *zeros        wLenVINDTA        4 0
020400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020800040510     C                   z-add     1             wFlgVINDTA
020900040510     C                   leave
021000040510     C                   endif
021100040510     C                   enddo
021200010618     C*
021300040510     C                   if        vindta > *blanks AND
021400040510     C                             wFlgVINDTA = 1
021500010330     C                   add       1             rrnum
021600010330     C*
021700010601     C                   if        *in70 = *off and
021800010330     C                             (vinflg = *blanks
021900010330     C                              or vinflg = '0'
022000010330     C                              or vinflg = '2')
022100010330     C*
022200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022300010711     C                   if        vinflg = *blanks or vinflg = '0'
022400010711     C                   clear                   vinmsg
022500010711     C                   endif
022600010601     C*
022700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022800070925     C                   eval      vindta  = ' ' + %trim(vindta)
022900070925     C*
023000080527     C                   exsr      inzvar
023100100318     C                   exsr      normCSV
023200010605     C                   exsr      impvab
023300010601     C*
023400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023500010605     C                   eval      depfil = VABLNP
023600010601     C                   exsr      repfil
023700010601     C                   if        depfil = invfil
023800021025     C                   if        vlrpoi = 999
023900030822     C                   move(P)   invfil        VABFGS
024000021025     C                   else
024100030822     C                   move(P)   vlrpoi        VABFGS
024200021025     C                   endif
024300010601     C*
024400010601     C                   exsr      PREELA
024500010601     C*
024600010604     C* Ebbene...
024700010604     C*
024800040119     C  N31              add       1             §CTROKVB
024900040119     C   32              add       1             §CTRMO
025000040119     C   31              add       1             §CTRNO
025100080418     C*
025200030822     C                   if        wGiro = 2
025300080513     C* Scrivo sempre il VAT
025400080513     C  N31              exsr      wrivat
025500110201     C                   exsr      chkvab
025600080513     C                   endif
025700010604     C*
025800010604     C                   if        *in31 = *off and
025900010604     C                             *in32 = *off
026000010604     C                   eval      vinflg = '1'
026100010604     C                   else
026200010604     C                   eval      vinflg = '2'
026300010604     C                   endif
026400010604     C                   endif
026500010601     C*
026600010604     C                   endif
026700010604     C*
026800010330     C                   else
026900010330     C                   eval      vinflg = '1'
027000010330     C                   endif
027100010601     C*
027200010601     C  N70              update    tivin000
027300010330     C*
027400030822     C  N70              enddo
027500040119     C*
027600080527     C* Verifico dati rimasti "in cannna"...
027700080527     C                   seton                                        65
027800110201     C                   exsr      chkvab
027900080527     C*
028000040119     C                   endif                                                  (endif - chkcall)
028100010601     C*
028200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028300010601     C                   if        cntNonEl = *zeros or
028400010601     C                             flgMulti = '0'
028500010330     C* Se non ci sono record con errori ...
028600010601     C                   if        §ctrno = 0 and
028700010604     C                             §ctrmo = 0 and
028800010601     C                             flgStato <> '2'
028900010330     C* ... restituisco esito OK.
029000010330     C                   eval      wrkesito = '0'
029100010330     C                   else
029200040119     C                   if        §ctrokvb > 0
029300010330     C                   eval      wrkesito = '1'
029400010330     C                   else
029500010615     C                   if        flgOk = '0'
029600010615     C                   eval      wrkesito = '2'
029700010615     C                   else
029800010615     C                   eval      wrkesito = '6'
029900010615     C                   endif
030000010330     C                   endif
030100010330     C                   endif
030200010601     C                   else
030300010601     C                   eval      wrkesito = '9'
030400010601     C                   endif
030500010330     C*
030600010330     C                   if        %open(tivin00r)
030700010330     C                   close     tivin00r
030800010330     C                   endif
030900021025     C                   if        %open(fivabwwr)
031000021025     C                   close     fivabwwr
031100010330     C                   endif
031200040512     C                   if        %open(fivatwwr)
031300040512     C                   close     fivatwwr
031400040512     C                   endif
031500010601     C*
031600010601     C                   if        vlrpoi <> 999
031700010601     C                   eval      invfil = vlrpoi
031800010601     C                   endif
031900010330     C*
032000040119     C                   if        §ctrokvb > 0
032100010601     C                             and invfil > *zeros
032200010330     C                   exsr      invio
032300010330     C                   endif
032400010601     C*
032500010618     C                   if        flgGiro = '1'
032600010601     C                   exsr      endela
032700010618     C                   endif
032800010330     C*
032900010330     C                   ENDSR
033000010330     C***
033100080527
033200080527
033300080527
033400080527     C*----------------------------------------------------*
033500080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033600080527     C*----------------------------------------------------*
033700120509     C     CHKVAB        BEGSR
033800110201     C*
033900110201     C* Se richiesti ignorare colli / peso / volume
034000110315 xxx C***                if        *in35
034100110315 xxx C***                z-add     *zeros        VABNCL
034200110315 xxx C***                z-add     *zeros        VABPKB
034300110315 xxx C***                z-add     *zeros        VABVLB
034400110315 xxx C***                endif
034500080527     C*
034600120509     C* Scrivo il VAB solo a rottura d spedizione
034700080527     C                   if        depspe = *zeros
034800080527     C  N31              eval      fivabds_sav = fivabds
034900080527     C                   eval      depspe = vabNSP
035000080527     C                   else
035100080527     C                   if        depspe = vabNSP AND *in65 = *off
035200100413     C   51              add       VABNCL        sav_VABNCL
035300100413     C   51              add       VABPKB        sav_VABPKB
035400100413     C   51              add       VABVLB        sav_VABVLB
035500100720     C   52              z-add     VABNCL        sav_VABNCL
035600100720     C   52              z-add     VABPKB        sav_VABPKB
035700100720     C   52              z-add     VABVLB        sav_VABVLB
035800100720     C   53              add       VABNCL        sav_VABNCL
035900100720     C   53              z-add     VABPKB        sav_VABPKB
036000100720     C   53              z-add     VABVLB        sav_VABVLB
036100080527     C                   else
036200080527     C  N31              eval      fivabds_ok = fivabds_sav
036300080527     C  N31              exsr      wrivab
036400080527     C  N31              eval      fivabds_sav = fivabds
036500080527     C                   eval      depspe = vabNSP
036600080527     C                   endif
036700080527     C                   endif
036800080527     C*
036900080527     C                   ENDSR
037000040119
037100040119
037200040119
037300040119     C*----------------------------------------------------*
037400040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037500040119     C*----------------------------------------------------*
037600040510     C     WRIVAB        BEGSR
037700080513     C*
037800080513     C* Gestisco l'eventuale rottura x numero spedizione
037900120716     C                   if        VABNSP <> depspe or *in65 = *on
038000070301     C                   if        wVATNOT_E <> *blanks
038100120716     C                   eval      ok_VABCTM = '7Q'
038200070301     C                   endif
038300100413     C                   write     fivab000
038400080513     C                   endif
038500040119     C*
038600040119     C                   ENDSR
038700040119
038800040119
038900040119
039000040119     C*----------------------------------------------------*
039100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039200040119     C*----------------------------------------------------*
039300040510     C     WRIVAT        BEGSR
039400060512     C*
039500060512     C* Inizializzo i campi chiave
039600060512     C                   eval      VATFGS = VABFGS
039700060512     C                   eval      VATAAS = VABAAS
039800060512     C                   eval      VATCCM = VABCCM
039900070823     C                   eval      VATNRS = VABNRS
040000060512     C                   eval      VATNSP = VABNSP
040100060512     C                   eval      VATLNP = VABLNP
040200040119     C*
040300040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040400040510     C                   if        VATTRC = *blanks
040500040510     C                   eval      VATTRC = 'E'
040600040510     C                   endif
040700110318     C                   if        VATNOT <> *blanks
040800040510     C                   write     FIVAT000
040900040119     C                   add       1             §CTROKVT
041000110318     C                   endif
041100060331     C*
041200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041300060331     C                   if        wVATNOT_A <> *blanks
041400060331     C                   eval      VATNOT = wVATNOT_A
041500060331     C                   eval      VATTRC = 'A'
041600060331     C                   write     FIVAT000
041700060331     C                   add       1             §CTROKVT
041800060331     C                   endif
041900060331     C*
042000060331     C                   if        wVATNOT_B <> *blanks
042100060331     C                   eval      VATNOT = wVATNOT_B
042200060331     C                   eval      VATTRC = 'B'
042300060331     C                   write     FIVAT000
042400060331     C                   add       1             §CTROKVT
042500060331     C                   endif
042600060331     C*
042700060331     C                   if        wVATNOT_E <> *blanks
042800120716     C***                eval      VATNOT = wVATNOT_E
042900060331     C                   eval      VATTRC = 'E'
043000120524     C*
043100120524     C* Sviluppati i "CHI SONO" in relazione al numero colli
043200120524     C*
043300120716     C     1             DO        VABNCL        wNumCollo         3 0
043400120716     C*** da analisi del POC il progressivo colli è max 3 char e deve contiene gli zeri non
043500120524     C*** significativi
043600120716     C                   EVAL      VATNOT = %subst(wVATNOT_E:1:11) +
043700120716     C                                %editc(wNumCollo:'X') +
043800120716     C                                %subst(wVATNOT_E:15:1)
043900120410     C                   write     FIVAT000
044000120410     C                   add       1             §CTROKVT
044100120524     C                   ENDDO
044200120524     C*
044300120320     C                   endif
044400120320     C*
044500121025     C* e-mail
044600110927     C                   if        wVATNOT_I <> *blanks
044700110927     C                   eval      VATNOT = wVATNOT_I
044800110927     C                   eval      VATTRC = 'I'
044900110927     C                   write     FIVAT000
045000110927     C                   add       1             §CTROKVT
045100110927     C                   endif
045200121025     C*
045300121025     C* e-mail 2
045400121025     C                   if        wVATNOT_J <> *blanks
045500121025     C                   eval      VATNOT = wVATNOT_J
045600121025     C                   eval      VATTRC = 'J'
045700121025     C                   write     FIVAT000
045800121025     C                   add       1             §CTROKVT
045900121025     C                   endif
046000150504     C*
046100150504     C                   if        wVATNOT_S <> *blanks
046200150504     C                   eval      VATNOT = wVATNOT_S
046300150504     C* i 2 flag devono essere blank
046400150504     C                   EVAL      %subst(VATNOT:17:1)=*blank
046500150504     C                   EVAL      %subst(VATNOT:18:1)=*blank
046600150504     C                   eval      VATTRC = 'S'
046700150504     C                   write     FIVAT000
046800150504     C                   add       1             §CTROKVT
046900150504     C                   endif
047000040119     C*
047100040119     C                   ENDSR
047200040119
047300040119
047400040119
047500040119     C*----------------------------------------------------*
047600040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047700040119     C*----------------------------------------------------*
047800040119     C     PREVAT        BEGSR
047900040119     C*
048000040119     C* Compongo il nome del membro da dare al FIVATWWR
048100040119     C                   eval      parmbr = vlrhdl
048200040119     C                   movel     'M'           parmbr
048300040510     C                   eval      parccm = vlrksc
048400040119     C                   eval      paropz = '1'
048500040119     C* Effettuo la chiamata al CLLE preposto
048600040119     C                   call(e)   'TITVVTC'
048700040119     C                   parm                    parccm
048800040119     C                   parm                    parmbr
048900040119     C                   parm                    paropz
049000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049100040119     C                   if        %error
049200040119     C                   movel     '1'           chkcall
049300040119     C                   else
049400040119     C                   movel     '0'           chkcall
049500040119     C                   endif
049600040119     C*
049700040119     C                   ENDSR
049800010601
049900010601
050000010601
050100010330     C*----------------------------------------------------*
050200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050300010330     C*----------------------------------------------------*
050400040119     C     INZVAR        BEGSR
050500080527     C*
050600080527     C                   setoff                                       65
050700010330     C*
050800030822     C* Inizializzo variabili di wrk
050900020204     C                   Z-ADD     *zeros        Num5_0            5 0
051000101018     C                   MOVEL     *blanks       wFlgCAS
051100060331     C                   MOVEL     *blanks       wVATNOT_A
051200060331     C                   MOVEL     *blanks       wVATNOT_B
051300060331     C                   MOVEL     *blanks       wVATNOT_E
051400110927     C                   MOVEL     *blanks       wVATNOT_I
051500121025     C                   MOVEL     *blanks       wVATNOT_J
051600121025     C                   MOVEL     *blanks       wVATNOT_IJ
051700150504     C                   MOVEL     *blanks       wVATNOT_S
051800120411     C                   MOVEL     *blanks       wNOT
051900120411     C                   MOVEL     *blanks       Data8A
052000030822     C*
052100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052200030822     C                   CLEAR                   SkSplitCSV
052300030822     C                   EVAL      i = 1
052400030822     C                   EVAL      posDa = *zeros
052500030822     C                   EVAL      posA  = *zeros
052600080527     C*
052700080527     C                   CLEAR                   FIVABDS
052800080527     C                   CLEAR                   FIVAT000
052900030822     C*
053000030822     C* Reimposto i valori di default
053100030822     C                   EXSR      DEFCAM
053200030822     C*
053300010330     C                   ENDSR
053400010330     C*----------------------------------------------------*
053500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053600010330     C*----------------------------------------------------*
053700010330     C     DEFCAM        BEGSR
053800030822     C*
053900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054000030715     C* e delimitatore testo.
054100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054400100318     C*
054500100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
054600100318     C                   EVAL      CharSOS = CharNUM
054700030822     C*
054800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
054900100720     C                   SETOFF                                       505152
055000100720     C                   SETOFF                                       53
055100100413     C                   SELECT
055200100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055300100720     C                   SETON                                        50
055400100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
055500100413     C                   SETON                                        51
055600100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
055700100720     C                   SETON                                        52
055800100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
055900100720     C                   SETON                                        53
056000100413     C                   ENDSL
056100030822     C*
056200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056300030822     C                   EVAL      posDaDft = 1
056400030822     C                   EVAL      posADft  = 0
056500030822     C                   EVAL      wGiroDft = 0
056600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056700030822     C                             posDaDft > 0
056800030822     C*
056900030822     C* Gestisco il 1° giro
057000030822     C                   IF        wGiroDft = 0
057100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057300030822     C* Incremento il contatore dei "giri"
057400030822     C                   EVAL      wGiroDft = 1
057500030822     C                   ELSE
057600030822     C                   EVAL      posDaDft = posADft
057700030822     C                   ENDIF
057800030822     C* Eseguo lo scan x trovare la fine del campo corrente
057900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058000030822     C*
058100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058300030822     C                   IF        posDaDft > 0 AND
058400030822     C                             posADft  > 0
058500030822     C* NCL
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'NCL'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiInt=*on
059500030822     C                   Z-ADD     PiVal         VABNCL
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* CCM
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'CCM'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiInt=*on
060800030822     C                   Z-ADD     PiVal         VABCCM
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* LNP
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'LNP'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiInt=*on
062100030822     C                   Z-ADD     PiVal         VABLNP
062200030822     C                   ENDIF
062300030822     C                   ENDIF
062400030822     C* NRS
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'NRS'
062900030822     C                   EVAL      PiStr=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   EXSR      CHKNUM
063300030822     C                   IF        PiInt=*on
063400030822     C                   Z-ADD     PiVal         VABNRS
063500030822     C                   ENDIF
063600030822     C                   ENDIF
063700030822     C* CTR
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'CTR'
064200030822     C                   EVAL      PiStr=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   EXSR      CHKNUM
064600030822     C                   IF        PiInt=*on
064700030822     C                   Z-ADD     PiVal         VABCTR
064800030822     C                   ENDIF
064900030822     C                   ENDIF
065000030822     C* PKB
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'PKB'
065500030822     C                   EVAL      PiStr=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   EXSR      CHKNUM
065900030822     C                   IF        PiNum=*on
066000120418     C                   Z-ADD     PiVal         VABPKB
066100030822     C                   ENDIF
066200030822     C                   ENDIF
066300030822     C* VLB
066400030822     C                   IF        %subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):1:3)
066700030822     C                             = 'VLB'
066800030822     C                   EVAL      PiStr=%trim(%subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100030822     C                   EXSR      CHKNUM
067200030822     C                   IF        PiNum=*on
067300030822     C                   Z-ADD     PiVal         VABVLB
067400030822     C                   ENDIF
067500030822     C                   ENDIF
067600030822     C* QFT
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'QFT'
068100030822     C                   EVAL      PiStr=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   EXSR      CHKNUM
068500030822     C                   IF        PiNum=*on
068600030822     C                   Z-ADD     PiVal         VABQFT
068700030822     C                   ENDIF
068800030822     C                   ENDIF
068900030822     C* CBO
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'CBO'
069400030822     C                   EVAL      VABCBO=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800030822     C* TSP
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'TSP'
070300030822     C                   EVAL      VABTSP=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700030822     C* VAS
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'VAS'
071200030822     C                   EVAL      VABVAS=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600030822     C* VCA
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'VCA'
072100030822     C                   EVAL      VABVCA=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500070925     C* TIC
072600070925     C                   IF        %subst(
072700070925     C                             %subst(vlrppt:posDaDft+1:
072800070925     C                             posADft-posDaDft-1):1:3)
072900070925     C                             = 'TIC'
073000070925     C                   EVAL      VABTIC=%trim(%subst(
073100070925     C                             %subst(vlrppt:posDaDft+1:
073200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300070925     C                   ENDIF
073400030822     C* GCA
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'GCA'
073900030822     C                   EVAL      VABGCA=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* CTM
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'CTM'
074800030822     C                   EVAL      VABCTM=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* FFD
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'FFD'
075700030822     C                   EVAL      VABFFD=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* VAD
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'VAD'
076600030822     C                   EVAL      VABVAD=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000030822     C* GMA
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'GMA'
077500030822     C                   EVAL      VABGMA=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900030822     C* GGA
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'GGA'
078400030822     C                   EVAL      VABGGA=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800030822     C* GVA
078900030822     C                   IF        %subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):1:3)
079200030822     C                             = 'GVA'
079300030822     C                   EVAL      VABGVA=%trim(%subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600030822     C                   ENDIF
079700030822     C* TC1
079800030822     C                   IF        %subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):1:3)
080100030822     C                             = 'TC1'
080200030822     C                   EVAL      VABTC1=%trim(%subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500030822     C                   ENDIF
080600030822     C* TC2
080700030822     C                   IF        %subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):1:3)
081000030822     C                             = 'TC2'
081100030822     C                   EVAL      VABTC2=%trim(%subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400030822     C                   ENDIF
081500040714     C* VATTRC
081600040714     C                   IF        %subst(
081700040714     C                             %subst(vlrppt:posDaDft+1:
081800040714     C                             posADft-posDaDft-1):1:3)
081900040714     C                             = 'TRC'
082000040714     C                   EVAL      VATTRC=%trim(%subst(
082100040714     C                             %subst(vlrppt:posDaDft+1:
082200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082300040714     C                   ENDIF
082400030822     C* ...
082500030822     C                   ENDIF
082600030822     C                   ENDDO
082700020204     C*
082800010330     C                   ENDSR
082900010607     C*----------------------------------------------------*
083000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083100010607     C*----------------------------------------------------*
083200010607     C     IMPVAB        BEGSR
083300010607     C*
083400010607     C                   SETOFF                                       3132
083500110315 xxx C***                SETON                                        35
083600030515     C*
083700010607     C                   MOVEL     datcor        VABAAS
083800010607     C                   MOVE      datcor        VABMGS
083900010607     C*
084000030715     C**********
084100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084200030715     C**********
084300030715     C                   IF        wGiro = *zeros
084400040113     C*
084500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084700080429     C                             %trim(intcol(3))+%trim(intcol(4))+
084800080429     C                             %trim(intcol(5))<>*blanks
084900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085000080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085100080429     C                                      %trim(intcol(5))
085200070823     C     ';':CharCSV   XLATE     vindta        vindta
085300040510     C                   ENDIF
085400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085500031201     C     minu:maiu     XLATE     vindta        vindta
085600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085700030715     C                   DOW       posDa <= %len(%trim(vindta))
085800030715     C*
085900030715     C* Gestisco il 1° campo
086000030715     C                   IF        i = 1
086100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086200030715     C                   EVAL      posDa = 1
086300030715     C* Eseguo lo scan x trovare la fine del primo campo
086400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086500030715     C* A questo punto "estraggo" il campo corrente
086600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086700030715     C                                                    (posA-posDa))
086800030715     C* X i campi successivi al 1°
086900030715     C                   ELSE
087000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087100030715     C                   EVAL      posDa = posA + 1
087200030715     C* Eseguo lo scan x trovare la fine del campo corrente
087300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087400030715     C* Gestisco l'ultimo campo
087500030715     C                   IF        posA = *zeros
087600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087700030715     C* A questo punto "estraggo" il campo corrente
087800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087900030715     C                                                    (posA-posDa))
088000030715     C                   LEAVE
088100030715     C                   ELSE
088200030715     C* A questo punto "estraggo" il campo corrente
088300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088400030715     C                                                    (posA-posDa))
088500030715     C                   ENDIF
088600030715     C                   ENDIF
088700030715     C* Incremento il contatore d campo
088800030715     C                   EVAL      i = i +1
088900030715     C                   ENDDO
089000030715     C*
089100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089200030715     C* delimitazione testo
089300030715     C                   EVAL      i = 1
089400030715     C                   DOW       i < %elem(SkSplitFLD)
089500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089700030715     C                   EVAL      i = i + 1
089800030715     C                   ENDDO
089900030715     C                   EVAL      wGiro = 1
090000030715     C                   ELSE
090100030715     C                   EVAL      wGiro = 2
090200031204     C**********
090300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090400031204     C* d separatore campo
090500031204     C**********
090600031204     C                   EVAL      vindta = %trim(vindta)
090700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090800031204     C                   EVAL      vindta = %subst(vindta:2)
090900031204     C                   ENDDO
091000031204     C*
091100031204     C                   Z-ADD     *zeros        lunghInput        4 0
091200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091600110912     C                   ENDDO
091700030715     C**********
091800030715     C* Effettuo lo split del campo dati d input
091900030715     C**********
092000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092100030715     C                   DOW       posDa <= %len(%trim(vindta))
092200030715     C*
092300030715     C* Gestisco il 1° campo
092400030715     C                   IF        i = 1
092500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092600030715     C                   EVAL      posDa = 1
092700030715     C* Eseguo lo scan x trovare la fine del primo campo
092800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092900030715     C* A questo punto "estraggo" il campo corrente
093000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093100030715     C                                                    (posA-posDa))
093200030715     C* X i campi successivi al 1°
093300030715     C                   ELSE
093400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093500030715     C                   EVAL      posDa = posA + 1
093600031204     C* Verifico che nn vi sia il campo nn valorizzato
093700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093800031204     C* Se campo nn valorizzato skippo al prossimo
093900031204     C                   EVAL      SkSplitCSV(i) = *blanks
094000031204     C                   EVAL      posA  = posA + 1
094100031204     C                   ELSE
094200030715     C* Eseguo lo scan x trovare la fine del campo corrente
094300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094400030715     C* Gestisco l'ultimo campo
094500030715     C                   IF        posA = *zeros
094600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094700030715     C* A questo punto "estraggo" il campo corrente
094800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094900030715     C                                                    (posA-posDa))
095000030715     C                   LEAVE
095100030715     C                   ELSE
095200030715     C* A questo punto "estraggo" il campo corrente
095300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095400030715     C                                                    (posA-posDa))
095500030715     C                   ENDIF
095600031204     C                   ENDIF
095700030715     C                   ENDIF
095800030715     C* Incremento il contatore d campo
095900030715     C                   EVAL      i = i +1
096000030715     C                   ENDDO
096100030715     C*
096200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096300030715     C* delimitazione testo
096400030715     C                   EVAL      i = 1
096500030715     C                   DOW       i < %elem(SkSplitCSV)
096600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096800030715     C                   EVAL      i = i + 1
096900030715     C                   ENDDO
097000100720     C*
097100130111     C* Stacco un progressivo x numero spedizione, in quanto NSP = RMN
097200130111 xxx C                   EXSR      repNSP
097300100326     C*
097400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097500030715     C                   EVAL      i = 1
097600030715     C                   DOW       i < %elem(SkSplitFLD)
097700030715     C*
097800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098000030715     C                             SkSplitCSV(i) <> *blanks
098100030721     C*** ATB
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098300030721     C                   EVAL      VABATB = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030715     C*** RSD
098600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099000030715     C* ==
099100030715     C                   ENDIF
099200030721     C*** RD2
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030715     C*** IND
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099800030715     C                   EVAL      VABIND = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030715     C*** LOD
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** PRD
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030721     C*** NZD
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030715     C*** RMA
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030715     C*** NOT
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101900030715     C                   ENDIF
102000030715     C*** NT2
102100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102300030715     C                   ENDIF
102400030715     C*** VCA
102500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102700030715     C                   ENDIF
102800030715     C*** VAS
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030715     C*** FFD
103300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103500030715     C                   ENDIF
103600030721     C*** GC1
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** GC2
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** TSP
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104700030721     C                   ENDIF
104800040510     C*** CBO
104900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105100040510     C                   ENDIF
105200030721     C*** NAS
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** TIC
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** GCA
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** XCO
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** CTM
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** TCR
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** CTS
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** FTM
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** VAD
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** GMA
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** GGA
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** GVA
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** TC1
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** TC2
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** SCL
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** RMO
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** NMO
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*
112100030721     C* Reperisco quindi i campi numerici...
112200040510     C*** NSP
112300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112400040510     C                   EVAL      PiStr=SkSplitCSV(i)
112500040510     C                   EXSR      CHKNUM
112600040510     C                   IF        PiInt=*on
112700040510     C                   Z-ADD     PiVal         VABNSP
112800040510     C                   ELSE
112900040510     C                   SETON                                        31
113000040510     C                   EVAL      VABNSP = *zeros
113100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113200040510     C                             + ' ' + 'VABNSP'
113300040510     C                   ENDIF
113400040510     C                   ENDIF
113500071106     C*** RMN
113600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113700071106     C                   EVAL      PiStr=SkSplitCSV(i)
113800071106     C                   EXSR      CHKNUM
113900071106     C                   IF        PiInt=*on
114000071106     C                   Z-ADD     PiVal         VABRMN
114100071106     C                   ELSE
114200071106     C                   SETON                                        32
114300071106     C                   EVAL      VABRMN = *zeros
114400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114500071106     C                             + ' ' + 'VABRMN'
114600071106     C                   ENDIF
114700071106     C                   ENDIF
114800030721     C*** CAD
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiInt=*on
115300030721     C                   Z-ADD     PiVal         Num5_0
115400030721     C                   MOVEL(p)  Num5_0        VABCAD
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      VABCAD = *zeros
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900030721     C                             + ' ' + 'VABCAD'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200030721     C*** NCL
116300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116400030721     C                   EVAL      PiStr=SkSplitCSV(i)
116500030721     C                   EXSR      CHKNUM
116600030721     C                   IF        PiInt=*on
116700030721     C                   Z-ADD     PiVal         VABNCL
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      VABNCL = *zeros
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200040122     C                             + ' ' + 'VABNCL'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** PKB
117600120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
117700120418     C* per cui si arrotonda il valore passato agli etti
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiNum=*on
118200120418     C                   Z-ADD(H)  PiVal         VABPKB
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      VABPKB = *zeros
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABPKB'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** CAS
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiNum=*on
119500030721     C                   Z-ADD     PiVal         VABCAS
119600120716     C                   IF        VABCAS > 0
119700141028     C                   EVAL      VABVCA = 'EUR'
119800141028     C                   EVAL      VABTIC = 'TM'
119900120716     C                   EVAL      wFlgCAS = '1'
120000120716     C                   ENDIF
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      VABCAS = *zeros
120400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120500030721     C                             + ' ' + 'VABCAS'
120600030721     C                   ENDIF
120700030721     C                   ENDIF
120800030721     C*** CCM
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121000030721     C                   EVAL      PiStr=SkSplitCSV(i)
121100030721     C                   EXSR      CHKNUM
121200030721     C                   IF        PiInt=*on      AND
121300030721     C                             PiVal<=9999999 AND
121400030721     C                             PiVal>*zeros
121500030721     C                   Z-ADD     PiVal         VABCCM
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      VABCCM = *zeros
121900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122000040510     C                             + ' ' + 'VABCCM'
122100030721     C                   ENDIF
122200030721     C                   ENDIF
122300030721     C*** LNP
122400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiInt=*on    AND
122800030721     C                             PiVal<=999   AND
122900030721     C                             PiVal>*zeros
123000030721     C                   Z-ADD     PiVal         VABLNP
123100030721     C                   ELSE
123200040715     C                   SETON                                        32
123300030721     C                   EVAL      VABLNP = *zeros
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500040510     C                             + ' ' + 'VABLNP'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** NRS
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on AND
124300030721     C                             PiVal<=99
124400030721     C                   Z-ADD     PiVal         VABNRS
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABNRS'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** LNA
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiInt=*on    AND
125600030721     C                             PiVal<=999   AND
125700030721     C                             PiVal>*zeros
125800030721     C                   Z-ADD     PiVal         VABLNA
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      VABLNA = *zeros
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABLNA'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** CTR
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on  AND
127100030721     C                             PiVal<=999
127200030721     C                   Z-ADD     PiVal         VABCTR
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABCTR'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** IAS
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiNum=*on
128400030721     C                   Z-ADD     PiVal         VABIAS
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABIAS'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030721     C*** VLB
129200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
129300030721     C                   EVAL      PiStr=SkSplitCSV(i)
129400030721     C                   EXSR      CHKNUM
129500030721     C                   IF        PiNum=*on
129600030721     C                   Z-ADD     PiVal         VABVLB
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABVLB'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300030721     C*** QFT
130400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiNum=*on
130800030721     C                   Z-ADD     PiVal         VABQFT
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABQFT'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** NCD
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiInt=*on      AND
132000030721     C                             PiVal<=9999999
132100030721     C                   Z-ADD     PiVal         VABNCD
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABNCD'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800030721     C*** NCA
132900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiInt=*on      AND
133300030721     C                             PiVal<=9999999
133400030721     C                   Z-ADD     PiVal         VABNCA
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABNCA'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** ZNC
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiInt=*on AND
134600030721     C                             PiVal<=99
134700030721     C                   Z-ADD     PiVal         VABZNC
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135100030721     C                             + ' ' + 'VABZNC'
135200030721     C                   ENDIF
135300030721     C                   ENDIF
135400030721     C*** DCR
135500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
135600030721     C                   EVAL      PiStr=SkSplitCSV(i)
135700030721     C                   EXSR      CHKNUM
135800030721     C                   IF        PiInt=*on       AND
135900030721     C                             PiVal<=99999999
136000030721     C                   Z-ADD     PiVal         VABDCR
136100030721     C                   ELSE
136200030721     C                   SETON                                        32
136300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136400030721     C                             + ' ' + 'VABDCR'
136500030721     C                   ENDIF
136600030721     C                   ENDIF
136700030721     C*** HCR
136800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136900030721     C                   EVAL      PiStr=SkSplitCSV(i)
137000030721     C                   EXSR      CHKNUM
137100030721     C                   IF        PiInt=*on   AND
137200030721     C                             PiVal<=9999
137300030721     C                   Z-ADD     PiVal         VABHCR
137400030721     C                   ELSE
137500030721     C                   SETON                                        32
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700030721     C                             + ' ' + 'VABHCR'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000030721     C*** VMD
138100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
138200030721     C                   EVAL      PiStr=SkSplitCSV(i)
138300030721     C                   EXSR      CHKNUM
138400030721     C                   IF        PiNum=*on
138500030721     C                   Z-ADD     PiVal         VABVMD
138600030721     C                   ELSE
138700030721     C                   SETON                                        32
138800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138900030721     C                             + ' ' + 'VABVMD'
139000030721     C                   ENDIF
139100030721     C                   ENDIF
139200030721     C*** ANT
139300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
139400030721     C                   EVAL      PiStr=SkSplitCSV(i)
139500030721     C                   EXSR      CHKNUM
139600030721     C                   IF        PiInt=*on        AND
139700030721     C                             PiVal<=999999999
139800030721     C                   Z-ADD     PiVal         VABANT
139900030721     C                   ELSE
140000030721     C                   SETON                                        32
140100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140200030721     C                             + ' ' + 'VABANT'
140300030721     C                   ENDIF
140400030721     C                   ENDIF
140500030721     C*** CMO
140600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
140700030721     C                   EVAL      PiStr=SkSplitCSV(i)
140800030721     C                   EXSR      CHKNUM
140900030721     C                   IF        PiInt=*on
141000030721     C                   Z-ADD     PiVal         Num5_0
141100030721     C                   MOVEL(p)  Num5_0        VABCMO
141200030721     C                   ELSE
141300030721     C                   SETON                                        32
141400030721     C                   EVAL      VABCMO = *zeros
141500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141600030721     C                             + ' ' + 'VABCMO'
141700030721     C                   ENDIF
141800030721     C                   ENDIF
141900040510     C*
142000040510     C* Reperisco i campi del FIVAT
142100040510     C*** VATNOT
142200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
142300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
142400040510     C                   ENDIF
142500040510     C*** VATTRC
142600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
142700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
142800040510     C                   ENDIF
142900060331     C*** VATNOT_A
143000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
143100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
143200060331     C                   ENDIF
143300060331     C*** VATNOT_B
143400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
143500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
143600060331     C                   ENDIF
143700060331     C*** VATNOT_E
143800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144000060331     C                   ENDIF
144100110927     C*** VATNOT_I
144200110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
144300110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
144400110927     C                   ENDIF
144500150504     C*** VATNOT_S
144600150504     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
144700150504     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
144800150504     C                   ENDIF
144900070823     C*
145000100831     C***  ===>  Gestione campi "particolari" dentro ciclo
145100120716
145200120716     C*** W_NOTE
145300120716     C                   IF        %trim(SkSplitFLD(i)) = 'W_NOTE'
145400130607     C                   EVAL      w100 = %trim(SkSplitCSV(i))
145500130607     C                   IF        %subst(w100:1:2) = 'A1'
145600130607     C                   EVAL      VABTC1 = 'A'
145700130607     C                   EVAL      %subst(w100:1:2) = *blank
145800130607     C                   EVAL      w100 = %trim(w100)
145900130607     C                   ENDIF
146000130607     C                   EVAL      VABNOT = %subst(w100:1:35)
146100130607     C                   EVAL      VABNT2 = %subst(w100:36:35)
146200120716     C                   ENDIF
146300120716
146400120716     C*** W_VABRMN
146500120716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
146600130111     C                   EVAL      PiStr=SkSplitCSV(i)
146700120716     C                   EXSR      CHKNUM
146800120716     C                   IF        PiInt=*on
146900120716     C                   Z-ADD     PiVal         VABRMN
147000130111     C***                Z-ADD     PiVal         VABNSP
147100120716     C                   ELSE
147200120716     C                   SETON                                        32
147300120716     C                   EVAL      VABRMN = *zeros
147400120716     C                   EVAL      vinmsg = %trimr(vinmsg)
147500130111     C                             + ' ' + 'VABRMN'
147600130111     C***                          + ' ' + 'VABRMN VABNSP'
147700120716     C                   ENDIF
147800120716     C                   ENDIF
147900120716
148000120716     C*** W_VATNOT_E
148100120716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
148200120716     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
148300120716     C                   ENDIF
148400120716
148500121025     C*** W_VATNOT_I
148600121025     C* al massimo 40 char
148700121025     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
148800121025     C                   EVAL      wVATNOT_IJ =
148900121025     C                                     %trim(SkSplitCSV(i))
149000121025     C                   ENDIF
149100150504
149200150504     C*** W_VATNOTS1
149300150504     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTS1'
149400150504     C                   EVAL      wS1 = %trim(SkSplitCSV(i))
149500150504     C                   ENDIF
149600150504     C*** W_VATNOTS2
149700150504     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTS2'
149800150504     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
149900150504     C                   ENDIF
150000121025
150100070925     C*
150200071029     C***  <===  -----------------------------------------
150300040510     C*
150400030715     C                   ENDIF
150500030715     C                   EVAL      i = i + 1
150600030715     C                   ENDDO
150700070925     C*
150800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
150900120411     C*
151000150504
151100150504     C*** Se valore presente è diverso da "DN" e diverso da "GD" allora occorre importare come _S
151200150504     C*** il valore _S2
151300150504     C                   IF        wS1 = 'DN' or wS1 = 'GD'
151400150504     C                   EVAL      wVATNOT_S = *blank
151500150504     C                   ENDIF
151600121025     C*
151700130219     C* se il c/assegno non è valorizzato, non va spedita l'email
151800130219     C                   IF        wFlgCAS <> '1'
151900130219     C                   EVAL      wVATNOT_IJ = *blanks
152000130219     C                   ENDIF
152100121025     C                   IF        %trim(wVATNOT_IJ) <> *blanks
152200121025     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
152300121025     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:5)
152400121025     C                   ENDIF
152500120509
152600070925     C*
152700071029     C***  <===  -----------------------------------------
152800070925     C*
152900070925     C* Considerazioni finali su CBO/CAS
153000100318     C                   IF        wFlgCAS = '1'    OR
153100100318     C                             VABCAS  > *zeros
153200070925     C                   IF        VABCBO = '1'
153300070925     C                   EVAL      VABCBO = '4'
153400070925     C                   ENDIF
153500070925     C                   IF        VABCBO = '2'
153600070925     C                   EVAL      VABCBO = '6'
153700070925     C                   ENDIF
153800070925     C                   ENDIF
153900070925     C***  <===  ----------------------------
154000040510     C*
154100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154200040510     C                   IF        VABNSP = *zeros
154300040510     C                   SETON                                        31
154400040510     C                   ENDIF
154500040715     C*
154600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
154700040715     C                   IF        VABLNP = *zeros
154800040715     C                   SETON                                        31
154900040715     C                   ENDIF
155000040510     C*
155100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155200120411     C                   IF        VABRMN = *zeros
155300120411     C                   Z-ADD     VABNSP        VABRMN
155400120411     C                   ENDIF
155500120328     C* IN QUESTO CASO LO FORZO = 1
155600120411     C***                IF        VABRMN = *zeros
155700120411     C***                Z-ADD     1             VABRMN
155800120411     C***                ENDIF
155900040510     C*
156000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
156100040510     C                   IF        VABRMA = *blanks
156200040714     C                   MOVEL     VABRMN        VABRMA
156300040510     C                   ENDIF
156400040128     C*
156500040510     C* Se provincia nn valorizzata la reperisco
156600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156700110912     C                   IF        VABPRD =  *blanks AND
156800110912     C                             VABNZD =  *blanks AND
156900110912     C                             VABCAD <> *blanks AND
157000110912     C                             VABLOD <> *blanks
157100040128     C                   CLEAR                   TISI95DS
157200040128     C                   EVAL      I95TCN = '3'
157300040128     C                   Z-ADD     datcor        I95DAT
157400040128     C                   EVAL      I95NAR = VABNZD
157500040128     C                   EVAL      I95CAP = VABCAD
157600040128     C                   EVAL      I95LOC = VABLOD
157700040128     C                   CALL      'TISI95R'
157800040128     C                   PARM                    TISI95DS
157900040128     C                   EVAL      VABPRD = O95PRV
158000040510     C                   ENDIF
158100020204     C*
158200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
158300020204     C                   EXSR      CHKIMPDIV
158400030715     C                   ENDIF
158500020204     C*
158600010607     C                   ENDSR
158700010607     C*----------------------------------------------------*
158800010601
158900020204
159000020204     C*----------------------------------------------------*
159100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159200020204     C*----------------------------------------------------*
159300020204     C     CHKIMPDIV     BEGSR
159400020204     C*
159500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
159600020204     C                   Z-ADD     *zeros        wrkDec            9 9
159700020204     C*
159800020204     C* Come prima cosa effettuo considerazioni sulla divisa
159900020204     C                   IF        vabIAS > *zeros
160000020204     C                   IF        vabVAS <> 'EUR'
160100020204     C                   EVAL      vabVAS =  'ITL'
160200020204     C                   ENDIF
160300020204     C                   ENDIF
160400020204     C*
160500020204     C                   IF        vabCAS > *zeros
160600020204     C                   IF        vabVCA <> 'EUR'
160700020204     C                   EVAL      vabVCA =  'ITL'
160800020204     C                   ENDIF
160900020204     C                   ENDIF
161000020204     C*
161100020204     C                   IF        vabVMD > *zeros
161200020204     C                   IF        vabVAD <> 'EUR'
161300020204     C                   EVAL      vabVAD =  'ITL'
161400020204     C                   ENDIF
161500020204     C                   ENDIF
161600020204     C*
161700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
161800020204     C                   Z-ADD     vabIAS        wrkDec
161900020204     C                   IF        wrkDec > *zeros
162000020204     C                   IF        vabVAS = 'ITL'
162100020204     C                   EVAL      vabIAS = *zeros
162200020204     C                   ENDIF
162300020204     C                   ENDIF
162400020204     C*
162500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
162600020204     C                   Z-ADD     vabCAS        wrkDec
162700020204     C                   IF        wrkDec > *zeros
162800020204     C                   IF        vabVCA = 'ITL'
162900020204     C                   EVAL      vabCAS = *zeros
163000020204     C                   ENDIF
163100020204     C                   ENDIF
163200020204     C*
163300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
163400020204     C                   Z-ADD     vabVMD        wrkDec
163500020204     C                   IF        wrkDec > *zeros
163600020204     C                   IF        vabVAD = 'ITL'
163700020204     C                   EVAL      vabVMD = *zeros
163800020204     C                   ENDIF
163900020204     C                   ENDIF
164000020204     C*
164100020204     C                   ENDSR
164200020204     C***
164300010330
164400100318
164500100318
164600100318     C*----------------------------------------------------*
164700100318     C*  CONTROLLO NUMERICITA' CAMPI
164800100318     C*----------------------------------------------------*
164900100318     C     CHKNUM        BEGSR
165000100318     C*
165100100318     C                   IF        PiDecChr = *blanks
165200100318     C                   EVAL      PiDecChr = CharNUM
165300100318     C                   ENDIF
165400100318     C*
165500100318     C                   callp     UBISNUM_Check(PiStr
165600100318     C                                          :PiDecChr
165700100318     C                                          :PiVal
165800100318     C                                          :PiNum
165900100318     C                                          :PiInt)
166000100318     C*
166100100318     C                   ENDSR
166200100318     C***
166300010601
166400010601
166500010601
166600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
166700010601     C     repfil        BEGSR
166800010601     C*
166900010601     C                   if        invfil = *zeros and
167000010601     C                             depfil > *zeros and
167100010629     C                             (vinflg = *blanks or
167200010629     C                              vinflg = *zeros)
167300010601     C
167400010601     C                   eval      invfil = depfil
167500010601     C                   endif
167600010601     C*
167700010601     C                   if        depfil <> invfil and
167800010601     C                             invfil > *zeros
167900010601     C                   eval      flgMulti = '1'
168000010601     C                   if        vinflg = *blanks
168100010601     C                   add       1             cntNonEl
168200010601     C                   endif
168300010601     C                   endif
168400010601     C*
168500010601     C                   if        vinflg = '2'
168600010601     C                   eval      flgStato = '2'
168700010601     C                   endif
168800010601     C*
168900010601     C                   ENDSR
169000010601     C***
169100010601
169200010601
169300010601
169400010330
169500040119      /TITLE Invio dei dati al punto operativo.
169600040119     C     invio         BEGSR
169700040119     C*
169800040119     C* 1° invio FIVAT
169900040119     C                   reset                   dscmz
170000040119     C                   move      vlrpoi        cmzdst
170100040119     C                   eval      cmzfld = 'FIVATWWR'
170200040119     C                   eval      cmzmbd = vlrhdl
170300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170400040119     C***                if        prmfir = *blanks
170500040119     C                   eval      cmzfla = 'FIVAT00F'
170600040119     C                   eval      cmzmba = 'FIVAT00F'
170700040119     C***                else
170800040119     C***                eval      cmzfla = prmfir
170900040119     C***                eval      cmzmba = prmfir
171000040119     C***                endif
171100040119     C                   eval      cmznrr = *zeros
171200040119     C                   move      §ctrokvt      cmznrr
171300040119     C                   eval      cmzlba = vlrfl1
171400040119     C                   call(e)   'TIS711C'
171500040119     C                   parm                    dscmz
171600040119     C                   parm      *blanks       esito
171700040119     C                   if        %error
171800040119     C                             or cmzerr = '1'
171900040119     C                             or esito  = '1'
172000040119     C                   eval      wrkesito = '3'
172100040119     C                   else
172200040119     C*
172300040119     C* 2° invio FIVAB
172400040119     C                   reset                   dscmz
172500040119     C                   move      vlrpoi        cmzdst
172600040119     C                   eval      cmzfld = vlrfou
172700040119     C                   eval      cmzmbd = vlrhdl
172800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172900040119     C***                if        prmfir = *blanks
173000040119     C                   eval      cmzfla = 'FIVAB00F'
173100040119     C                   eval      cmzmba = 'FIVAB00F'
173200040119     C***                else
173300040119     C***                eval      cmzfla = prmfir
173400040119     C***                eval      cmzmba = prmfir
173500040119     C***                endif
173600040119     C                   eval      cmznrr = *zeros
173700040119     C                   move      §ctrokvb      cmznrr
173800040119     C                   eval      cmzlba = vlrfl1
173900040119     C                   call(e)   'TIS711C'
174000040119     C                   parm                    dscmz
174100040119     C                   parm      *blanks       esito
174200040119     C                   if        %error
174300040119     C                             or cmzerr = '1'
174400040119     C                             or esito  = '1'
174500040119     C                   eval      wrkesito = '3'
174600040119     C                   endif
174700040119     C                   endif
174800040119     C*
174900040119     C                   ENDSR
175000040119     C***
175100010601
175200010601
175300010601
175400010601
175500010601      /TITLE Invio dei dati al punto operativo.
175600010601     C     opeini        BEGSR
175700010601     C*
175800010601     C* Inizializzo flag e contatori operativi
175900010601     C                   movel     '0'           flgGiro           1
176000010601     C                   movel     '0'           flgMulti          1
176100010601     C                   movel     '1'           flgStato          1
176200010615     C                   movel     '0'           flgOk             1
176300010601     C                   z-add     *zeros        cntNonEl         10 0
176400010601     C                   z-add     *zeros        depfil            3 0
176500010601     C                   z-add     *zeros        invfil            3 0
176600010601     C*
176700010601     C                   ENDSR
176800010601     C***
176900070326
177000070326
177100070326
177200070326
177300070326     C     *pssr         BEGSR
177400070329     C*
177500070329     C                   if        %open(tivin00r)
177600070329     C                   close     tivin00r
177700070329     C                   endif
177800070329     C                   if        %open(fivabwwr)
177900070329     C                   close     fivabwwr
178000070329     C                   endif
178100070329     C                   if        %open(fivatwwr)
178200070329     C                   close     fivatwwr
178300070329     C                   endif
178400070326     C*
178500070326     C* Effettuo la chiamata al CLLE preposto
178600070326     C                   call(e)   'TITVVTC'
178700070326     C                   parm                    parccm
178800070326     C                   parm                    parmbr
178900070326     C                   parm      '2'           paropz
179000070326     C*
179100070326     C                   eval      wrkesito = '2'
179200070404     C*
179300070404     C                   seton                                        LR
179400070326     C*
179500070326     C                   ENDSR     '*CANCL'
179600070326     C***
179700100318
179800100318
179900100318
180000100318     C     normCSV       BEGSR
180100100318     C*
180200100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
180300100318     C* separatori campi
180400100318     C                   if        CharTXT <> *blanks
180500100318     C                   z-add     *zeros        wLenVINDTA        4 0
180600100318     C                   z-add     *zeros        wFlgVINDTA        1 0
180700100318     C                   setoff                                       80
180800100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
180900100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
181000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
181100100318     C                   if        not *in80
181200100318     C                   seton                                        80
181300100318     C                   else
181400100318     C                   setoff                                       80
181500100318     C                   endif
181600100318     C                   endif
181700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
181800100318     C                             and *in80 = *on
181900100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
182000100318     C                   endif
182100100318     C                   enddo
182200100318     C                   endif
182300100318     C*
182400100318     C                   ENDSR
182500100318     C***
182600070326
182700100326
182800100326
182900100326     C*----------------------------------------------------*
183000100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
183100100326     C*----------------------------------------------------*
183200100326     C     REPNSP        BEGSR
183300100326     C*
183400100326     C* NSP => Stacco un numeratore da AZNUM
183500100326     C                   clear                   TRUL33DS
183600100326     C                   eval      I33OPE = *zeros
183700100326     C                   eval      I33CNU = 302
183800100326     C                   eval      I33NUM = 1
183900100326     C                   movel     TRUL33DS      KPJBU
184000100326     C                   call      'TRUL33R'
184100100326     C                   parm                    KPJBA
184200100326     C                   movel     KPJBU         TRUL33DS
184300100326     C                   if        O33ERR = *zeros
184400100326     C                   z-add     O33NRF        VABNSP
184500100326     C                   z-add     O33NRF        VATNSP
184600100326     C                   else
184700100326     C                   SETON                                        31
184800100326     C                   EVAL      vinmsg = %trimr(vinmsg)
184900120716     C                             + ' ' + 'VABNSP VATNSP'
185000100326     C                   endif
185100100326     C*
185200100326     C                   ENDSR
185300010330
185400100326
185500010330
185600000613     C     *inzsr        BEGSR
185700990910     C*
185800990910     C     *entry        plist
185900990920     C                   parm                    tivlrds
186000990921     C                   parm      wrkesito      esito
186100000724     C                   parm                    prmlit
186200000710     C                   parm                    prmfir
186300010330     C*
186400010330     C* CALCOLA LA DATA CORRENTE
186500100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186600100318     C                   eval      datcor = %dec(%date() : *ISO)
186700000613     C*
186800000613     C                   ENDSR
186900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
187000130111W_VABRMN;*;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;VABPKB;VABNCL;VABCAS;W_NOTE;*;VABRMA;
187100150504W_VATNOT_I;W_VATNOT_E;X;W_VATNOTS1;W_VATNOTS2;
187200120418
187300111116
187400100318
