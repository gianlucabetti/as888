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
008500171215     D wBancali        s                   LIKE(VABNCL)
008600030822     D*------------------
008700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008800030822     D*------------------
008900030822     D posDaDft        S              3  0 INZ(*zeros)
009000030822     D posADft         S              3  0 INZ(*zeros)
009100030822     D j               s              3  0 INZ(1)
009200030822     D wGiroDft        s              1  0 INZ(*zeros)
009300080418     D*------------------
009400080418     D* Costanti
009500080418     D*------------------
009600080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009700080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009800100318
009900100318     D*------------------
010000100318     D* LINKING A DEFINIZIONI ESTERNE
010100100318     D*------------------
010200100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010300100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010400100318
010500010330
010600010330
010700010330
010800000913     C                   reset                   rrnum
010900990921     C                   reset                   esito
011000990921     C                   reset                   wrkesito
011100010601     C*
011200010601     C                   exsr      opeini
011300010605     C                   exsr      rwvab
011400010601     C*
011500010601     C                   seton                                        lr
011600010601
011700010601
011800010601
011900010601     C*--------------------------------------------------------
012000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012100010601     C*--------------------------------------------------------
012200010601     C     PREELA        BEGSR
012300010601     C*
012400010601     C* SE OCCORRE SPEDIRE IN FILIALE
012500010601     C                   if        invfil <> *zeros and
012600010601     C                             flgGiro = '0'
012700010601     C*
012800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012900010601     C                   eval      flgGiro = '1'
013000010601     C*
013100010601     C                   endif
013200010601     C*
013300010601     C                   ENDSR
013400010601     C***
013500010601
013600010601
013700010601
013800010601
013900010601     C*--------------------------------------------------------
014000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014100010601     C*--------------------------------------------------------
014200010601     C     ENDELA        BEGSR
014300040119     C*
014400040119     C* Effettuo la chiamata al CLLE preposto
014500040119     C                   call(e)   'TITVVTC'
014600040510     C                   parm                    parccm
014700040119     C                   parm                    parmbr
014800040119     C                   parm      '2'           paropz
014900050201     C*
015000050201     C* Effettuo lancio TISI95 solo x chiusura
015100050201     C                   CLEAR                   TISI95DS
015200050201     C                   EVAL      I95TLA = 'C'
015300050201     C                   CALL      'TISI95R'
015400050201     C                   PARM                    TISI95DS
015500000616     C*
015600010601     C                   ENDSR
015700010601
015800010601
015900010330     C*--------------------------------------------------------
016000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016100010330     C*--------------------------------------------------------
016200010605     C     RWVAB         BEGSR
016300010330     C*
016400010330     C                   if        not %open(tivin00r)
016500010330     C                   open      tivin00r
016600010330     C                   endif
016700021025     C                   if        not %open(fivabwwr)
016800021025     C                   open      fivabwwr
016900010330     C                   endif
017000040119     C*
017100040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017200040119     C                   exsr      prevat
017300040119     C*
017400040119     C                   if        chkcall = '0'
017500040119     C*
017600040119     C                   if        not %open(fivatwwr)
017700040119     C                   open      fivatwwr
017800040119     C                   endif
017900010330     C*
018000040119     C                   clear                   §CTROKVB          7 0
018100040119     C                   clear                   §CTROKVT          7 0
018200040119     C                   clear                   §CTRMO            7 0
018300040119     C                   clear                   §CTRNO            7 0
018400120410     C*
018500120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018600120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018700120410     C                   IF        Intestazioni = *off
018800120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018900120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
019000120410     C                   exsr      inzvar
019100120410     C                   exsr      impvab
019200120410     C                   ENDIF
019300010330     C*
019400030822     C                   do        *HIVAL
019500010330     C*
019600030822     C                   read      tivin00r                               70
019700010618     C*
019800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019900010618     C                   if        vinflg = '1'
020000010618     C                   eval      flgOk = '1'
020100010618     C                   endif
020200040510     C*
020300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020400040510     C                   z-add     *zeros        wLenVINDTA        4 0
020500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020900040510     C                   z-add     1             wFlgVINDTA
021000040510     C                   leave
021100040510     C                   endif
021200040510     C                   enddo
021300010618     C*
021400040510     C                   if        vindta > *blanks AND
021500040510     C                             wFlgVINDTA = 1
021600010330     C                   add       1             rrnum
021700010330     C*
021800010601     C                   if        *in70 = *off and
021900010330     C                             (vinflg = *blanks
022000010330     C                              or vinflg = '0'
022100010330     C                              or vinflg = '2')
022200010330     C*
022300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022400010711     C                   if        vinflg = *blanks or vinflg = '0'
022500010711     C                   clear                   vinmsg
022600010711     C                   endif
022700010601     C*
022800070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022900070925     C                   eval      vindta  = ' ' + %trim(vindta)
023000070925     C*
023100080527     C                   exsr      inzvar
023200100318     C                   exsr      normCSV
023300010605     C                   exsr      impvab
023400010601     C*
023500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023600010605     C                   eval      depfil = VABLNP
023700010601     C                   exsr      repfil
023800010601     C                   if        depfil = invfil
023900021025     C                   if        vlrpoi = 999
024000030822     C                   move(P)   invfil        VABFGS
024100021025     C                   else
024200030822     C                   move(P)   vlrpoi        VABFGS
024300021025     C                   endif
024400010601     C*
024500010601     C                   exsr      PREELA
024600010601     C*
024700010604     C* Ebbene...
024800010604     C*
024900040119     C  N31              add       1             §CTROKVB
025000040119     C   32              add       1             §CTRMO
025100040119     C   31              add       1             §CTRNO
025200080418     C*
025300030822     C                   if        wGiro = 2
025400080513     C* Scrivo sempre il VAT
025500080513     C  N31              exsr      wrivat
025600110201     C                   exsr      chkvab
025700080513     C                   endif
025800010604     C*
025900010604     C                   if        *in31 = *off and
026000010604     C                             *in32 = *off
026100010604     C                   eval      vinflg = '1'
026200010604     C                   else
026300010604     C                   eval      vinflg = '2'
026400010604     C                   endif
026500010604     C                   endif
026600010601     C*
026700010604     C                   endif
026800010604     C*
026900010330     C                   else
027000010330     C                   eval      vinflg = '1'
027100010330     C                   endif
027200010601     C*
027300010601     C  N70              update    tivin000
027400010330     C*
027500030822     C  N70              enddo
027600040119     C*
027700080527     C* Verifico dati rimasti "in cannna"...
027800080527     C                   seton                                        65
027900110201     C                   exsr      chkvab
028000080527     C*
028100040119     C                   endif                                                  (endif - chkcall)
028200010601     C*
028300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028400010601     C                   if        cntNonEl = *zeros or
028500010601     C                             flgMulti = '0'
028600010330     C* Se non ci sono record con errori ...
028700010601     C                   if        §ctrno = 0 and
028800010604     C                             §ctrmo = 0 and
028900010601     C                             flgStato <> '2'
029000010330     C* ... restituisco esito OK.
029100010330     C                   eval      wrkesito = '0'
029200010330     C                   else
029300040119     C                   if        §ctrokvb > 0
029400010330     C                   eval      wrkesito = '1'
029500010330     C                   else
029600010615     C                   if        flgOk = '0'
029700010615     C                   eval      wrkesito = '2'
029800010615     C                   else
029900010615     C                   eval      wrkesito = '6'
030000010615     C                   endif
030100010330     C                   endif
030200010330     C                   endif
030300010601     C                   else
030400010601     C                   eval      wrkesito = '9'
030500010601     C                   endif
030600010330     C*
030700010330     C                   if        %open(tivin00r)
030800010330     C                   close     tivin00r
030900010330     C                   endif
031000021025     C                   if        %open(fivabwwr)
031100021025     C                   close     fivabwwr
031200010330     C                   endif
031300040512     C                   if        %open(fivatwwr)
031400040512     C                   close     fivatwwr
031500040512     C                   endif
031600010601     C*
031700010601     C                   if        vlrpoi <> 999
031800010601     C                   eval      invfil = vlrpoi
031900010601     C                   endif
032000010330     C*
032100040119     C                   if        §ctrokvb > 0
032200010601     C                             and invfil > *zeros
032300010330     C                   exsr      invio
032400010330     C                   endif
032500010601     C*
032600010618     C                   if        flgGiro = '1'
032700010601     C                   exsr      endela
032800010618     C                   endif
032900010330     C*
033000010330     C                   ENDSR
033100010330     C***
033200080527
033300080527
033400080527
033500080527     C*----------------------------------------------------*
033600080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033700080527     C*----------------------------------------------------*
033800120509     C     CHKVAB        BEGSR
033900110201     C*
034000110201     C* Se richiesti ignorare colli / peso / volume
034100110315 xxx C***                if        *in35
034200110315 xxx C***                z-add     *zeros        VABNCL
034300110315 xxx C***                z-add     *zeros        VABPKB
034400110315 xxx C***                z-add     *zeros        VABVLB
034500110315 xxx C***                endif
034600080527     C*
034700120509     C* Scrivo il VAB solo a rottura d spedizione
034800080527     C                   if        depspe = *zeros
034900080527     C  N31              eval      fivabds_sav = fivabds
035000080527     C                   eval      depspe = vabNSP
035100080527     C                   else
035200080527     C                   if        depspe = vabNSP AND *in65 = *off
035300100413     C   51              add       VABNCL        sav_VABNCL
035400100413     C   51              add       VABPKB        sav_VABPKB
035500100413     C   51              add       VABVLB        sav_VABVLB
035600100720     C   52              z-add     VABNCL        sav_VABNCL
035700100720     C   52              z-add     VABPKB        sav_VABPKB
035800100720     C   52              z-add     VABVLB        sav_VABVLB
035900100720     C   53              add       VABNCL        sav_VABNCL
036000100720     C   53              z-add     VABPKB        sav_VABPKB
036100100720     C   53              z-add     VABVLB        sav_VABVLB
036200080527     C                   else
036300080527     C  N31              eval      fivabds_ok = fivabds_sav
036400080527     C  N31              exsr      wrivab
036500080527     C  N31              eval      fivabds_sav = fivabds
036600080527     C                   eval      depspe = vabNSP
036700080527     C                   endif
036800080527     C                   endif
036900080527     C*
037000080527     C                   ENDSR
037100040119
037200040119
037300040119
037400040119     C*----------------------------------------------------*
037500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037600040119     C*----------------------------------------------------*
037700040510     C     WRIVAB        BEGSR
037800080513     C*
037900080513     C* Gestisco l'eventuale rottura x numero spedizione
038000120716     C                   if        VABNSP <> depspe or *in65 = *on
038100070301     C                   if        wVATNOT_E <> *blanks
038200120716     C                   eval      ok_VABCTM = '7Q'
038300070301     C                   endif
038400100413     C                   write     fivab000
038500080513     C                   endif
038600040119     C*
038700040119     C                   ENDSR
038800040119
038900040119
039000040119
039100040119     C*----------------------------------------------------*
039200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039300040119     C*----------------------------------------------------*
039400040510     C     WRIVAT        BEGSR
039500060512     C*
039600060512     C* Inizializzo i campi chiave
039700060512     C                   eval      VATFGS = VABFGS
039800060512     C                   eval      VATAAS = VABAAS
039900060512     C                   eval      VATCCM = VABCCM
040000070823     C                   eval      VATNRS = VABNRS
040100060512     C                   eval      VATNSP = VABNSP
040200060512     C                   eval      VATLNP = VABLNP
040300040119     C*
040400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040500040510     C                   if        VATTRC = *blanks
040600040510     C                   eval      VATTRC = 'E'
040700040510     C                   endif
040800110318     C                   if        VATNOT <> *blanks
040900040510     C                   write     FIVAT000
041000040119     C                   add       1             §CTROKVT
041100110318     C                   endif
041200060331     C*
041300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041400060331     C                   if        wVATNOT_A <> *blanks
041500060331     C                   eval      VATNOT = wVATNOT_A
041600060331     C                   eval      VATTRC = 'A'
041700060331     C                   write     FIVAT000
041800060331     C                   add       1             §CTROKVT
041900060331     C                   endif
042000060331     C*
042100060331     C                   if        wVATNOT_B <> *blanks
042200060331     C                   eval      VATNOT = wVATNOT_B
042300060331     C                   eval      VATTRC = 'B'
042400060331     C                   write     FIVAT000
042500060331     C                   add       1             §CTROKVT
042600060331     C                   endif
042700060331     C*
042800060331     C                   if        wVATNOT_E <> *blanks
042900120716     C***                eval      VATNOT = wVATNOT_E
043000060331     C                   eval      VATTRC = 'E'
043100120524     C*
043200120524     C* Sviluppati i "CHI SONO" in relazione al numero colli
043300120524     C*
043400120716     C     1             DO        VABNCL        wNumCollo         3 0
043500120716     C*** da analisi del POC il progressivo colli è max 3 char e deve contiene gli zeri non
043600120524     C*** significativi
043700120716     C                   EVAL      VATNOT = %subst(wVATNOT_E:1:11) +
043800120716     C                                %editc(wNumCollo:'X') +
043900120716     C                                %subst(wVATNOT_E:15:1)
044000120410     C                   write     FIVAT000
044100120410     C                   add       1             §CTROKVT
044200120524     C                   ENDDO
044300120524     C*
044400120320     C                   endif
044500120320     C*
044600121025     C* e-mail
044700110927     C                   if        wVATNOT_I <> *blanks
044800110927     C                   eval      VATNOT = wVATNOT_I
044900110927     C                   eval      VATTRC = 'I'
045000110927     C                   write     FIVAT000
045100110927     C                   add       1             §CTROKVT
045200110927     C                   endif
045300121025     C*
045400121025     C* e-mail 2
045500121025     C                   if        wVATNOT_J <> *blanks
045600121025     C                   eval      VATNOT = wVATNOT_J
045700121025     C                   eval      VATTRC = 'J'
045800121025     C                   write     FIVAT000
045900121025     C                   add       1             §CTROKVT
046000121025     C                   endif
046100150504     C*
046200150504     C                   if        wVATNOT_S <> *blanks
046300150504     C                   eval      VATNOT = wVATNOT_S
046400150504     C* i 2 flag devono essere blank
046500150504     C                   EVAL      %subst(VATNOT:17:1)=*blank
046600150504     C                   EVAL      %subst(VATNOT:18:1)=*blank
046700150504     C                   eval      VATTRC = 'S'
046800150504     C                   write     FIVAT000
046900150504     C                   add       1             §CTROKVT
047000150504     C                   endif
047100040119     C*
047200040119     C                   ENDSR
047300040119
047400040119
047500040119
047600040119     C*----------------------------------------------------*
047700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047800040119     C*----------------------------------------------------*
047900040119     C     PREVAT        BEGSR
048000040119     C*
048100040119     C* Compongo il nome del membro da dare al FIVATWWR
048200040119     C                   eval      parmbr = vlrhdl
048300040119     C                   movel     'M'           parmbr
048400040510     C                   eval      parccm = vlrksc
048500040119     C                   eval      paropz = '1'
048600040119     C* Effettuo la chiamata al CLLE preposto
048700040119     C                   call(e)   'TITVVTC'
048800040119     C                   parm                    parccm
048900040119     C                   parm                    parmbr
049000040119     C                   parm                    paropz
049100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049200040119     C                   if        %error
049300040119     C                   movel     '1'           chkcall
049400040119     C                   else
049500040119     C                   movel     '0'           chkcall
049600040119     C                   endif
049700040119     C*
049800040119     C                   ENDSR
049900010601
050000010601
050100010601
050200010330     C*----------------------------------------------------*
050300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050400010330     C*----------------------------------------------------*
050500040119     C     INZVAR        BEGSR
050600080527     C*
050700080527     C                   setoff                                       65
050800010330     C*
050900030822     C* Inizializzo variabili di wrk
051000020204     C                   Z-ADD     *zeros        Num5_0            5 0
051100101018     C                   MOVEL     *blanks       wFlgCAS
051200060331     C                   MOVEL     *blanks       wVATNOT_A
051300060331     C                   MOVEL     *blanks       wVATNOT_B
051400060331     C                   MOVEL     *blanks       wVATNOT_E
051500110927     C                   MOVEL     *blanks       wVATNOT_I
051600121025     C                   MOVEL     *blanks       wVATNOT_J
051700121025     C                   MOVEL     *blanks       wVATNOT_IJ
051800150504     C                   MOVEL     *blanks       wVATNOT_S
051900120411     C                   MOVEL     *blanks       wNOT
052000120411     C                   MOVEL     *blanks       Data8A
052100171215     C                   Z-ADD     *zeros        wBancali
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
066200120418     C                   Z-ADD     PiVal         VABPKB
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
067500030822     C                   Z-ADD     PiVal         VABVLB
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
097300130111     C* Stacco un progressivo x numero spedizione, in quanto NSP = RMN
097400130111 xxx C                   EXSR      repNSP
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
099300030715     C                   ENDIF
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
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiInt=*on
115500030721     C                   Z-ADD     PiVal         Num5_0
115600030721     C                   MOVEL(p)  Num5_0        VABCAD
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      VABCAD = *zeros
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABCAD'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
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
117800120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
117900120418     C* per cui si arrotonda il valore passato agli etti
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiNum=*on
118400120418     C                   Z-ADD(H)  PiVal         VABPKB
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      VABPKB = *zeros
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900030721     C                             + ' ' + 'VABPKB'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** CAS
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiNum=*on
119700030721     C                   Z-ADD     PiVal         VABCAS
119800120716     C                   IF        VABCAS > 0
119900141028     C                   EVAL      VABVCA = 'EUR'
120000141028     C                   EVAL      VABTIC = 'TM'
120100120716     C                   EVAL      wFlgCAS = '1'
120200120716     C                   ENDIF
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      VABCAS = *zeros
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABCAS'
120800030721     C                   ENDIF
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
129800030721     C                   Z-ADD     PiVal         VABVLB
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
144700150504     C*** VATNOT_S
144800150504     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
144900150504     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
145000150504     C                   ENDIF
145100070823     C*
145200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
145300120716
145400120716     C*** W_NOTE
145500120716     C                   IF        %trim(SkSplitFLD(i)) = 'W_NOTE'
145600130607     C                   EVAL      w100 = %trim(SkSplitCSV(i))
145700130607     C                   IF        %subst(w100:1:2) = 'A1'
145800130607     C                   EVAL      VABTC1 = 'A'
145900130607     C                   EVAL      %subst(w100:1:2) = *blank
146000130607     C                   EVAL      w100 = %trim(w100)
146100130607     C                   ENDIF
146200130607     C                   EVAL      VABNOT = %subst(w100:1:35)
146300130607     C                   EVAL      VABNT2 = %subst(w100:36:35)
146400120716     C                   ENDIF
146500120716
146600120716     C*** W_VABRMN
146700120716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
146800130111     C                   EVAL      PiStr=SkSplitCSV(i)
146900120716     C                   EXSR      CHKNUM
147000120716     C                   IF        PiInt=*on
147100120716     C                   Z-ADD     PiVal         VABRMN
147200130111     C***                Z-ADD     PiVal         VABNSP
147300120716     C                   ELSE
147400120716     C                   SETON                                        32
147500120716     C                   EVAL      VABRMN = *zeros
147600120716     C                   EVAL      vinmsg = %trimr(vinmsg)
147700130111     C                             + ' ' + 'VABRMN'
147800130111     C***                          + ' ' + 'VABRMN VABNSP'
147900120716     C                   ENDIF
148000120716     C                   ENDIF
148100120716
148200120716     C*** W_VATNOT_E
148300120716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
148400120716     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
148500120716     C                   ENDIF
148600120716
148700121025     C*** W_VATNOT_I
148800121025     C* al massimo 40 char
148900121025     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
149000121025     C                   EVAL      wVATNOT_IJ =
149100121025     C                                     %trim(SkSplitCSV(i))
149200121025     C                   ENDIF
149300150504
149400150504     C*** W_VATNOTS1
149500150504     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTS1'
149600150504     C                   EVAL      wS1 = %trim(SkSplitCSV(i))
149700150504     C                   ENDIF
149800150504     C*** W_VATNOTS2
149900150504     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTS2'
150000150504     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
150100150504     C                   ENDIF
150200171214
150300171215     C*** W_BANCALI
150400171214     C                   IF        %trim(SkSplitFLD(i)) = 'W_BANCALI'
150500171215     C                             and SkSplitCSV(i) <> *blank
150600171214     C                   EVAL      PiStr=SkSplitCSV(i)
150700171214     C                   EXSR      CHKNUM
150800171214     C                   IF        PiInt=*on
150900171215     C                   Z-ADD     PiVal         wBancali
151000171214     C                   ELSE
151100171215     C                   EVAL      wBancali = *zeros
151200171214     C                   ENDIF
151300171214     C                   ENDIF
151400121025
151500070925     C*
151600071029     C***  <===  -----------------------------------------
151700040510     C*
151800030715     C                   ENDIF
151900030715     C                   EVAL      i = i + 1
152000030715     C                   ENDDO
152100070925     C*
152200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
152300120411     C*
152400150504
152500150504     C*** Se valore presente è diverso da "DN" e diverso da "GD" allora occorre importare come _S
152600150504     C*** il valore _S2
152700150504     C                   IF        wS1 = 'DN' or wS1 = 'GD'
152800150504     C                   EVAL      wVATNOT_S = *blank
152900150504     C                   ENDIF
153000121025     C*
153100130219     C* se il c/assegno non è valorizzato, non va spedita l'email
153200130219     C                   IF        wFlgCAS <> '1'
153300130219     C                   EVAL      wVATNOT_IJ = *blanks
153400130219     C                   ENDIF
153500121025     C                   IF        %trim(wVATNOT_IJ) <> *blanks
153600121025     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
153700121025     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:5)
153800121025     C                   ENDIF
153900171215     C*
154000171215     C* se è impostato il valore dei bancali, questo sostituisce il nr. colli
154100171215     C                   IF        wBancali > 0
154200171215     C                   EVAL      VABNCL = wBancali
154300171215     C                   ENDIF
154400120509
154500070925     C*
154600071029     C***  <===  -----------------------------------------
154700070925     C*
154800070925     C* Considerazioni finali su CBO/CAS
154900100318     C                   IF        wFlgCAS = '1'    OR
155000100318     C                             VABCAS  > *zeros
155100070925     C                   IF        VABCBO = '1'
155200070925     C                   EVAL      VABCBO = '4'
155300070925     C                   ENDIF
155400070925     C                   IF        VABCBO = '2'
155500070925     C                   EVAL      VABCBO = '6'
155600070925     C                   ENDIF
155700070925     C                   ENDIF
155800070925     C***  <===  ----------------------------
155900040510     C*
156000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
156100040510     C                   IF        VABNSP = *zeros
156200040510     C                   SETON                                        31
156300040510     C                   ENDIF
156400040715     C*
156500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
156600040715     C                   IF        VABLNP = *zeros
156700040715     C                   SETON                                        31
156800040715     C                   ENDIF
156900040510     C*
157000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
157100120411     C                   IF        VABRMN = *zeros
157200120411     C                   Z-ADD     VABNSP        VABRMN
157300120411     C                   ENDIF
157400120328     C* IN QUESTO CASO LO FORZO = 1
157500120411     C***                IF        VABRMN = *zeros
157600120411     C***                Z-ADD     1             VABRMN
157700120411     C***                ENDIF
157800040510     C*
157900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
158000040510     C                   IF        VABRMA = *blanks
158100040714     C                   MOVEL     VABRMN        VABRMA
158200040510     C                   ENDIF
158300040128     C*
158400040510     C* Se provincia nn valorizzata la reperisco
158500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
158600110912     C                   IF        VABPRD =  *blanks AND
158700110912     C                             VABNZD =  *blanks AND
158800110912     C                             VABCAD <> *blanks AND
158900110912     C                             VABLOD <> *blanks
159000040128     C                   CLEAR                   TISI95DS
159100040128     C                   EVAL      I95TCN = '3'
159200040128     C                   Z-ADD     datcor        I95DAT
159300040128     C                   EVAL      I95NAR = VABNZD
159400040128     C                   EVAL      I95CAP = VABCAD
159500040128     C                   EVAL      I95LOC = VABLOD
159600040128     C                   CALL      'TISI95R'
159700040128     C                   PARM                    TISI95DS
159800040128     C                   EVAL      VABPRD = O95PRV
159900040510     C                   ENDIF
160000020204     C*
160100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
160200020204     C                   EXSR      CHKIMPDIV
160300030715     C                   ENDIF
160400020204     C*
160500010607     C                   ENDSR
160600010607     C*----------------------------------------------------*
160700010601
160800020204
160900020204     C*----------------------------------------------------*
161000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
161100020204     C*----------------------------------------------------*
161200020204     C     CHKIMPDIV     BEGSR
161300020204     C*
161400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
161500020204     C                   Z-ADD     *zeros        wrkDec            9 9
161600020204     C*
161700020204     C* Come prima cosa effettuo considerazioni sulla divisa
161800020204     C                   IF        vabIAS > *zeros
161900020204     C                   IF        vabVAS <> 'EUR'
162000020204     C                   EVAL      vabVAS =  'ITL'
162100020204     C                   ENDIF
162200020204     C                   ENDIF
162300020204     C*
162400020204     C                   IF        vabCAS > *zeros
162500020204     C                   IF        vabVCA <> 'EUR'
162600020204     C                   EVAL      vabVCA =  'ITL'
162700020204     C                   ENDIF
162800020204     C                   ENDIF
162900020204     C*
163000020204     C                   IF        vabVMD > *zeros
163100020204     C                   IF        vabVAD <> 'EUR'
163200020204     C                   EVAL      vabVAD =  'ITL'
163300020204     C                   ENDIF
163400020204     C                   ENDIF
163500020204     C*
163600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
163700020204     C                   Z-ADD     vabIAS        wrkDec
163800020204     C                   IF        wrkDec > *zeros
163900020204     C                   IF        vabVAS = 'ITL'
164000020204     C                   EVAL      vabIAS = *zeros
164100020204     C                   ENDIF
164200020204     C                   ENDIF
164300020204     C*
164400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
164500020204     C                   Z-ADD     vabCAS        wrkDec
164600020204     C                   IF        wrkDec > *zeros
164700020204     C                   IF        vabVCA = 'ITL'
164800020204     C                   EVAL      vabCAS = *zeros
164900020204     C                   ENDIF
165000020204     C                   ENDIF
165100020204     C*
165200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
165300020204     C                   Z-ADD     vabVMD        wrkDec
165400020204     C                   IF        wrkDec > *zeros
165500020204     C                   IF        vabVAD = 'ITL'
165600020204     C                   EVAL      vabVMD = *zeros
165700020204     C                   ENDIF
165800020204     C                   ENDIF
165900020204     C*
166000020204     C                   ENDSR
166100020204     C***
166200010330
166300100318
166400100318
166500100318     C*----------------------------------------------------*
166600100318     C*  CONTROLLO NUMERICITA' CAMPI
166700100318     C*----------------------------------------------------*
166800100318     C     CHKNUM        BEGSR
166900100318     C*
167000100318     C                   IF        PiDecChr = *blanks
167100100318     C                   EVAL      PiDecChr = CharNUM
167200100318     C                   ENDIF
167300100318     C*
167400100318     C                   callp     UBISNUM_Check(PiStr
167500100318     C                                          :PiDecChr
167600100318     C                                          :PiVal
167700100318     C                                          :PiNum
167800100318     C                                          :PiInt)
167900100318     C*
168000100318     C                   ENDSR
168100100318     C***
168200010601
168300010601
168400010601
168500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
168600010601     C     repfil        BEGSR
168700010601     C*
168800010601     C                   if        invfil = *zeros and
168900010601     C                             depfil > *zeros and
169000010629     C                             (vinflg = *blanks or
169100010629     C                              vinflg = *zeros)
169200010601     C
169300010601     C                   eval      invfil = depfil
169400010601     C                   endif
169500010601     C*
169600010601     C                   if        depfil <> invfil and
169700010601     C                             invfil > *zeros
169800010601     C                   eval      flgMulti = '1'
169900010601     C                   if        vinflg = *blanks
170000010601     C                   add       1             cntNonEl
170100010601     C                   endif
170200010601     C                   endif
170300010601     C*
170400010601     C                   if        vinflg = '2'
170500010601     C                   eval      flgStato = '2'
170600010601     C                   endif
170700010601     C*
170800010601     C                   ENDSR
170900010601     C***
171000010601
171100010601
171200010601
171300010330
171400040119      /TITLE Invio dei dati al punto operativo.
171500040119     C     invio         BEGSR
171600040119     C*
171700040119     C* 1° invio FIVAT
171800040119     C                   reset                   dscmz
171900040119     C                   move      vlrpoi        cmzdst
172000040119     C                   eval      cmzfld = 'FIVATWWR'
172100040119     C                   eval      cmzmbd = vlrhdl
172200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172300040119     C***                if        prmfir = *blanks
172400040119     C                   eval      cmzfla = 'FIVAT00F'
172500040119     C                   eval      cmzmba = 'FIVAT00F'
172600040119     C***                else
172700040119     C***                eval      cmzfla = prmfir
172800040119     C***                eval      cmzmba = prmfir
172900040119     C***                endif
173000040119     C                   eval      cmznrr = *zeros
173100040119     C                   move      §ctrokvt      cmznrr
173200040119     C                   eval      cmzlba = vlrfl1
173300040119     C                   call(e)   'TIS711C'
173400040119     C                   parm                    dscmz
173500040119     C                   parm      *blanks       esito
173600040119     C                   if        %error
173700040119     C                             or cmzerr = '1'
173800040119     C                             or esito  = '1'
173900040119     C                   eval      wrkesito = '3'
174000040119     C                   else
174100040119     C*
174200040119     C* 2° invio FIVAB
174300040119     C                   reset                   dscmz
174400040119     C                   move      vlrpoi        cmzdst
174500040119     C                   eval      cmzfld = vlrfou
174600040119     C                   eval      cmzmbd = vlrhdl
174700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
174800040119     C***                if        prmfir = *blanks
174900040119     C                   eval      cmzfla = 'FIVAB00F'
175000040119     C                   eval      cmzmba = 'FIVAB00F'
175100040119     C***                else
175200040119     C***                eval      cmzfla = prmfir
175300040119     C***                eval      cmzmba = prmfir
175400040119     C***                endif
175500040119     C                   eval      cmznrr = *zeros
175600040119     C                   move      §ctrokvb      cmznrr
175700040119     C                   eval      cmzlba = vlrfl1
175800040119     C                   call(e)   'TIS711C'
175900040119     C                   parm                    dscmz
176000040119     C                   parm      *blanks       esito
176100040119     C                   if        %error
176200040119     C                             or cmzerr = '1'
176300040119     C                             or esito  = '1'
176400040119     C                   eval      wrkesito = '3'
176500040119     C                   endif
176600040119     C                   endif
176700040119     C*
176800040119     C                   ENDSR
176900040119     C***
177000010601
177100010601
177200010601
177300010601
177400010601      /TITLE Invio dei dati al punto operativo.
177500010601     C     opeini        BEGSR
177600010601     C*
177700010601     C* Inizializzo flag e contatori operativi
177800010601     C                   movel     '0'           flgGiro           1
177900010601     C                   movel     '0'           flgMulti          1
178000010601     C                   movel     '1'           flgStato          1
178100010615     C                   movel     '0'           flgOk             1
178200010601     C                   z-add     *zeros        cntNonEl         10 0
178300010601     C                   z-add     *zeros        depfil            3 0
178400010601     C                   z-add     *zeros        invfil            3 0
178500010601     C*
178600010601     C                   ENDSR
178700010601     C***
178800070326
178900070326
179000070326
179100070326
179200070326     C     *pssr         BEGSR
179300070329     C*
179400070329     C                   if        %open(tivin00r)
179500070329     C                   close     tivin00r
179600070329     C                   endif
179700070329     C                   if        %open(fivabwwr)
179800070329     C                   close     fivabwwr
179900070329     C                   endif
180000070329     C                   if        %open(fivatwwr)
180100070329     C                   close     fivatwwr
180200070329     C                   endif
180300070326     C*
180400070326     C* Effettuo la chiamata al CLLE preposto
180500070326     C                   call(e)   'TITVVTC'
180600070326     C                   parm                    parccm
180700070326     C                   parm                    parmbr
180800070326     C                   parm      '2'           paropz
180900070326     C*
181000070326     C                   eval      wrkesito = '2'
181100070404     C*
181200070404     C                   seton                                        LR
181300070326     C*
181400070326     C                   ENDSR     '*CANCL'
181500070326     C***
181600100318
181700100318
181800100318
181900100318     C     normCSV       BEGSR
182000100318     C*
182100100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
182200100318     C* separatori campi
182300100318     C                   if        CharTXT <> *blanks
182400100318     C                   z-add     *zeros        wLenVINDTA        4 0
182500100318     C                   z-add     *zeros        wFlgVINDTA        1 0
182600100318     C                   setoff                                       80
182700100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
182800100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
182900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
183000100318     C                   if        not *in80
183100100318     C                   seton                                        80
183200100318     C                   else
183300100318     C                   setoff                                       80
183400100318     C                   endif
183500100318     C                   endif
183600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
183700100318     C                             and *in80 = *on
183800100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
183900100318     C                   endif
184000100318     C                   enddo
184100100318     C                   endif
184200100318     C*
184300100318     C                   ENDSR
184400100318     C***
184500070326
184600100326
184700100326
184800100326     C*----------------------------------------------------*
184900100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
185000100326     C*----------------------------------------------------*
185100100326     C     REPNSP        BEGSR
185200100326     C*
185300100326     C* NSP => Stacco un numeratore da AZNUM
185400100326     C                   clear                   TRUL33DS
185500100326     C                   eval      I33OPE = *zeros
185600100326     C                   eval      I33CNU = 302
185700100326     C                   eval      I33NUM = 1
185800100326     C                   movel     TRUL33DS      KPJBU
185900100326     C                   call      'TRUL33R'
186000100326     C                   parm                    KPJBA
186100100326     C                   movel     KPJBU         TRUL33DS
186200100326     C                   if        O33ERR = *zeros
186300100326     C                   z-add     O33NRF        VABNSP
186400100326     C                   z-add     O33NRF        VATNSP
186500100326     C                   else
186600100326     C                   SETON                                        31
186700100326     C                   EVAL      vinmsg = %trimr(vinmsg)
186800120716     C                             + ' ' + 'VABNSP VATNSP'
186900100326     C                   endif
187000100326     C*
187100100326     C                   ENDSR
187200010330
187300100326
187400010330
187500000613     C     *inzsr        BEGSR
187600990910     C*
187700990910     C     *entry        plist
187800990920     C                   parm                    tivlrds
187900990921     C                   parm      wrkesito      esito
188000000724     C                   parm                    prmlit
188100000710     C                   parm                    prmfir
188200010330     C*
188300010330     C* CALCOLA LA DATA CORRENTE
188400100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
188500100318     C                   eval      datcor = %dec(%date() : *ISO)
188600000613     C*
188700000613     C                   ENDSR
188800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
188900130111W_VABRMN;*;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;VABPKB;VABNCL;VABCAS;W_NOTE;*;VABRMA;
189000171214W_VATNOT_I;W_VATNOT_E;X;W_VATNOTS1;W_VATNOTS2;W_BANCALI
189100120418
189200111116
189300100318
