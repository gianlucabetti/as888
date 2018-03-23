000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120509      *
000300120509      * PARTICOLARITA':
000400120524      * Il file è una riga per spedizione = ordine AVON che è anche RMN,
000500120524      * per cui impostiamo NSP = RMN.
000600120524      * I CHI SONO si sviluppano così:
000700120524      * - 3 char fissi "332"
000800120524      * - 7 char con VABNSP
000900120524      * - 2 char con lo sviluppo da 1 a nr.colli.
001000120524      * Il telefono da riportare su VAT si compone da 2 colonne per cui sono state create le
001100120524      * variabili PrefixTel e Telefono
001200120509      *
001300121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001400120209     F*
001500990910     Ftivin00r  uF   E             DISK    usropn
001600100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001700040119     FFIVATwwr  O    E             DISK    usropn
001800000313     D*
001900040113     D*------------
002000040113     D* SCHIEDE A PROGRAMMA
002100040113     D*------------
002200080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000040128     D tisi95ds      e ds
003100100413     D fivabds       e ds                  extname(fivab00f)
003200100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003300100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003400010330     D esito           s              1
003500010330     D prmlit          s             10
003600010330     D prmfir          s             10
003700010330     D wrkesito        s                   like(esito)
003800010330     D rrnum           s              6  0 INZ(*zeros)
003900040510     D parccm          s              8    INZ(*blanks)
004000040119     D parmbr          s             10    INZ(*blanks)
004100040119     D paropz          s              1    INZ(*blanks)
004200040119     D chkcall         s              1    INZ(*blanks)
004300080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004400080418     D wFlgCAS         s              1    INZ(*blanks)
004500120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004600120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004700120524     D Intestazioni    s               n   inz(*off)
004800100326     D*------------------
004900100326     D* DS REPERIMENTO NUMERATORE
005000100326     D*------------------
005100100326     D trul33ds      e ds                  inz
005200100326     D*------------------
005300100326     D* DS ARCHITETTURA
005400100326     D*------------------
005500100326     D kpjba         e ds                  inz
005600100326
005700030715     D*------------------
005800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005900030715     D*------------------
006000030715     D SkSplitFLD      S             10    DIM(100)
006100030715     D SkSplitCSV      S            256    DIM(100)
006200030715     D CharCSV         S              1
006300030715     D CharTXT         S              1
006400030715     D CharNUM         S              1
006500100318     D CharSOS         S              1
006600030715     D posDa           S              3  0 INZ(*zeros)
006700030715     D posA            S              3  0 INZ(*zeros)
006800030715     D i               s              3  0 INZ(1)
006900030715     D wGiro           s              1  0 INZ(*zeros)
007000070823     D wVATNOT_A       s                   LIKE(VATNOT)
007100060331     D wVATNOT_B       s                   LIKE(VATNOT)
007200060331     D wVATNOT_E       s                   LIKE(VATNOT)
007300110927     D wVATNOT_I       s                   LIKE(VATNOT)
007400170920     D wVATNOT_S       s                   LIKE(VATNOT)
007500120411     D wNOT            s             70
007600120411     D Data8A          s              8
007700120524     D PrefixTel       s                   like(VATNOT)
007800120524     D Telefono        s                   like(VATNOT)
007900030822     D*------------------
008000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008100030822     D*------------------
008200030822     D posDaDft        S              3  0 INZ(*zeros)
008300030822     D posADft         S              3  0 INZ(*zeros)
008400030822     D j               s              3  0 INZ(1)
008500030822     D wGiroDft        s              1  0 INZ(*zeros)
008600080418     D*------------------
008700080418     D* Costanti
008800080418     D*------------------
008900080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009000080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009100100318
009200100318     D*------------------
009300100318     D* LINKING A DEFINIZIONI ESTERNE
009400100318     D*------------------
009500100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009600100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009700100318
009800010330
009900010330
010000010330
010100000913     C                   reset                   rrnum
010200990921     C                   reset                   esito
010300990921     C                   reset                   wrkesito
010400010601     C*
010500010601     C                   exsr      opeini
010600010605     C                   exsr      rwvab
010700010601     C*
010800010601     C                   seton                                        lr
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011400010601     C*--------------------------------------------------------
011500010601     C     PREELA        BEGSR
011600010601     C*
011700010601     C* SE OCCORRE SPEDIRE IN FILIALE
011800010601     C                   if        invfil <> *zeros and
011900010601     C                             flgGiro = '0'
012000010601     C*
012100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012200010601     C                   eval      flgGiro = '1'
012300010601     C*
012400010601     C                   endif
012500010601     C*
012600010601     C                   ENDSR
012700010601     C***
012800010601
012900010601
013000010601
013100010601
013200010601     C*--------------------------------------------------------
013300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013400010601     C*--------------------------------------------------------
013500010601     C     ENDELA        BEGSR
013600040119     C*
013700040119     C* Effettuo la chiamata al CLLE preposto
013800040119     C                   call(e)   'TITVVTC'
013900040510     C                   parm                    parccm
014000040119     C                   parm                    parmbr
014100040119     C                   parm      '2'           paropz
014200050201     C*
014300050201     C* Effettuo lancio TISI95 solo x chiusura
014400050201     C                   CLEAR                   TISI95DS
014500050201     C                   EVAL      I95TLA = 'C'
014600050201     C                   CALL      'TISI95R'
014700050201     C                   PARM                    TISI95DS
014800000616     C*
014900010601     C                   ENDSR
015000010601
015100010601
015200010330     C*--------------------------------------------------------
015300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015400010330     C*--------------------------------------------------------
015500010605     C     RWVAB         BEGSR
015600010330     C*
015700010330     C                   if        not %open(tivin00r)
015800010330     C                   open      tivin00r
015900010330     C                   endif
016000021025     C                   if        not %open(fivabwwr)
016100021025     C                   open      fivabwwr
016200010330     C                   endif
016300040119     C*
016400040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016500040119     C                   exsr      prevat
016600040119     C*
016700040119     C                   if        chkcall = '0'
016800040119     C*
016900040119     C                   if        not %open(fivatwwr)
017000040119     C                   open      fivatwwr
017100040119     C                   endif
017200010330     C*
017300040119     C                   clear                   §CTROKVB          7 0
017400040119     C                   clear                   §CTROKVT          7 0
017500040119     C                   clear                   §CTRMO            7 0
017600040119     C                   clear                   §CTRNO            7 0
017700120410     C*
017800120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017900120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018000120410     C                   IF        Intestazioni = *off
018100120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018200120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018300120410     C                   exsr      inzvar
018400120410     C                   exsr      impvab
018500120410     C                   ENDIF
018600010330     C*
018700030822     C                   do        *HIVAL
018800010330     C*
018900030822     C                   read      tivin00r                               70
019000010618     C*
019100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019200010618     C                   if        vinflg = '1'
019300010618     C                   eval      flgOk = '1'
019400010618     C                   endif
019500040510     C*
019600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019700040510     C                   z-add     *zeros        wLenVINDTA        4 0
019800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020200040510     C                   z-add     1             wFlgVINDTA
020300040510     C                   leave
020400040510     C                   endif
020500040510     C                   enddo
020600010618     C*
020700040510     C                   if        vindta > *blanks AND
020800040510     C                             wFlgVINDTA = 1
020900010330     C                   add       1             rrnum
021000010330     C*
021100010601     C                   if        *in70 = *off and
021200010330     C                             (vinflg = *blanks
021300010330     C                              or vinflg = '0'
021400010330     C                              or vinflg = '2')
021500010330     C*
021600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021700010711     C                   if        vinflg = *blanks or vinflg = '0'
021800010711     C                   clear                   vinmsg
021900010711     C                   endif
022000010601     C*
022100070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022200070925     C                   eval      vindta  = ' ' + %trim(vindta)
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
033100120509     C     CHKVAB        BEGSR
033200110201     C*
033300110201     C* Se richiesti ignorare colli / peso / volume
033400110315 xxx C***                if        *in35
033500110315 xxx C***                z-add     *zeros        VABNCL
033600110315 xxx C***                z-add     *zeros        VABPKB
033700110315 xxx C***                z-add     *zeros        VABVLB
033800110315 xxx C***                endif
033900080527     C*
034000120509     C* Scrivo il VAB solo a rottura d spedizione
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
037500070301     C                   eval      VABCTM = '7Q'
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
042400120524     C*
042500120524     C* Sviluppati i "CHI SONO" in relazione al numero colli
042600120524     C*
042700120524     C     1             DO        VABNCL        wNumCollo         2 0
042800120524     C*** da analisi del POC il progressivo colli è max 2 char e deve contiene gli zeri non
042900120524     C*** significativi
043000120524     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
043100120524     C                                %editc(wNumCollo:'X')
043200120410     C                   write     FIVAT000
043300120410     C                   add       1             §CTROKVT
043400120524     C                   ENDDO
043500120524     C*
043600120320     C                   endif
043700120320     C*
043800110927     C*
043900110927     C                   if        wVATNOT_I <> *blanks
044000110927     C                   eval      VATNOT = wVATNOT_I
044100110927     C                   eval      VATTRC = 'I'
044200110927     C                   write     FIVAT000
044300110927     C                   add       1             §CTROKVT
044400110927     C                   endif
044500170920     C*
044600170920     C                   if        wVATNOT_S <> *blanks
044700170920     C                   eval      VATNOT = wVATNOT_S
044800170920     C                   eval      VATTRC = 'S'
044900170920     C                   SELECT
045000170920     C                   WHEN      wVATNOT_I <> *blanks
045100170920     C* se ci sono sia l'email che il nr.cellulare imposto i flag ad abilitato alert SMS e
045200170920     C* disabilitato alert via email
045300170920     C                   EVAL      %subst(VATNOT:17:1)='S'
045400170920     C                   EVAL      %subst(VATNOT:18:1)='N'
045500170920     C                   OTHER
045600170920     C* se c'è solo il nr.cellulare
045700170920     C* i 2 flag devono essere blank
045800170920     C                   EVAL      %subst(VATNOT:17:1)=*blank
045900170920     C                   EVAL      %subst(VATNOT:18:1)=*blank
046000170920     C                   ENDSL
046100170920     C                   write     FIVAT000
046200170920     C                   add       1             §CTROKVT
046300170920     C                   endif
046400040119     C*
046500040119     C                   ENDSR
046600040119
046700040119
046800040119
046900040119     C*----------------------------------------------------*
047000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047100040119     C*----------------------------------------------------*
047200040119     C     PREVAT        BEGSR
047300040119     C*
047400040119     C* Compongo il nome del membro da dare al FIVATWWR
047500040119     C                   eval      parmbr = vlrhdl
047600040119     C                   movel     'M'           parmbr
047700040510     C                   eval      parccm = vlrksc
047800040119     C                   eval      paropz = '1'
047900040119     C* Effettuo la chiamata al CLLE preposto
048000040119     C                   call(e)   'TITVVTC'
048100040119     C                   parm                    parccm
048200040119     C                   parm                    parmbr
048300040119     C                   parm                    paropz
048400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048500040119     C                   if        %error
048600040119     C                   movel     '1'           chkcall
048700040119     C                   else
048800040119     C                   movel     '0'           chkcall
048900040119     C                   endif
049000040119     C*
049100040119     C                   ENDSR
049200010601
049300010601
049400010601
049500010330     C*----------------------------------------------------*
049600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049700010330     C*----------------------------------------------------*
049800040119     C     INZVAR        BEGSR
049900080527     C*
050000080527     C                   setoff                                       65
050100010330     C*
050200030822     C* Inizializzo variabili di wrk
050300020204     C                   Z-ADD     *zeros        Num5_0            5 0
050400101018     C                   MOVEL     *blanks       wFlgCAS
050500060331     C                   MOVEL     *blanks       wVATNOT_A
050600060331     C                   MOVEL     *blanks       wVATNOT_B
050700060331     C                   MOVEL     *blanks       wVATNOT_E
050800110927     C                   MOVEL     *blanks       wVATNOT_I
050900170920     C                   MOVEL     *blanks       wVATNOT_S
051000120411     C                   MOVEL     *blanks       wNOT
051100120411     C                   MOVEL     *blanks       Data8A
051200030822     C*
051300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051400030822     C                   CLEAR                   SkSplitCSV
051500030822     C                   EVAL      i = 1
051600030822     C                   EVAL      posDa = *zeros
051700030822     C                   EVAL      posA  = *zeros
051800080527     C*
051900080527     C                   CLEAR                   FIVABDS
052000080527     C                   CLEAR                   FIVAT000
052100030822     C*
052200030822     C* Reimposto i valori di default
052300030822     C                   EXSR      DEFCAM
052400030822     C*
052500010330     C                   ENDSR
052600010330     C*----------------------------------------------------*
052700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052800010330     C*----------------------------------------------------*
052900010330     C     DEFCAM        BEGSR
053000030822     C*
053100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
053200030715     C* e delimitatore testo.
053300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
053600100318     C*
053700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
053800100318     C                   EVAL      CharSOS = CharNUM
053900030822     C*
054000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
054100100720     C                   SETOFF                                       505152
054200100720     C                   SETOFF                                       53
054300100413     C                   SELECT
054400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
054500100720     C                   SETON                                        50
054600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
054700100413     C                   SETON                                        51
054800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
054900100720     C                   SETON                                        52
055000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
055100100720     C                   SETON                                        53
055200100413     C                   ENDSL
055300030822     C*
055400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055500030822     C                   EVAL      posDaDft = 1
055600030822     C                   EVAL      posADft  = 0
055700030822     C                   EVAL      wGiroDft = 0
055800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055900030822     C                             posDaDft > 0
056000030822     C*
056100030822     C* Gestisco il 1° giro
056200030822     C                   IF        wGiroDft = 0
056300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056500030822     C* Incremento il contatore dei "giri"
056600030822     C                   EVAL      wGiroDft = 1
056700030822     C                   ELSE
056800030822     C                   EVAL      posDaDft = posADft
056900030822     C                   ENDIF
057000030822     C* Eseguo lo scan x trovare la fine del campo corrente
057100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
057200030822     C*
057300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057500030822     C                   IF        posDaDft > 0 AND
057600030822     C                             posADft  > 0
057700030822     C* NCL
057800030822     C                   IF        %subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):1:3)
058100030822     C                             = 'NCL'
058200030822     C                   EVAL      PiStr=%trim(%subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500030822     C                   EXSR      CHKNUM
058600030822     C                   IF        PiInt=*on
058700030822     C                   Z-ADD     PiVal         VABNCL
058800030822     C                   ENDIF
058900030822     C                   ENDIF
059000030822     C* CCM
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'CCM'
059500030822     C                   EVAL      PiStr=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   EXSR      CHKNUM
059900030822     C                   IF        PiInt=*on
060000030822     C                   Z-ADD     PiVal         VABCCM
060100030822     C                   ENDIF
060200030822     C                   ENDIF
060300030822     C* LNP
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'LNP'
060800030822     C                   EVAL      PiStr=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   EXSR      CHKNUM
061200030822     C                   IF        PiInt=*on
061300030822     C                   Z-ADD     PiVal         VABLNP
061400030822     C                   ENDIF
061500030822     C                   ENDIF
061600030822     C* NRS
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'NRS'
062100030822     C                   EVAL      PiStr=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   EXSR      CHKNUM
062500030822     C                   IF        PiInt=*on
062600030822     C                   Z-ADD     PiVal         VABNRS
062700030822     C                   ENDIF
062800030822     C                   ENDIF
062900030822     C* CTR
063000030822     C                   IF        %subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):1:3)
063300030822     C                             = 'CTR'
063400030822     C                   EVAL      PiStr=%trim(%subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700030822     C                   EXSR      CHKNUM
063800030822     C                   IF        PiInt=*on
063900030822     C                   Z-ADD     PiVal         VABCTR
064000030822     C                   ENDIF
064100030822     C                   ENDIF
064200030822     C* PKB
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'PKB'
064700030822     C                   EVAL      PiStr=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   EXSR      CHKNUM
065100030822     C                   IF        PiNum=*on
065200120418     C                   Z-ADD     PiVal         VABPKB
065300030822     C                   ENDIF
065400030822     C                   ENDIF
065500030822     C* VLB
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'VLB'
066000030822     C                   EVAL      PiStr=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   EXSR      CHKNUM
066400030822     C                   IF        PiNum=*on
066500030822     C                   Z-ADD     PiVal         VABVLB
066600030822     C                   ENDIF
066700030822     C                   ENDIF
066800030822     C* QFT
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'QFT'
067300030822     C                   EVAL      PiStr=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   EXSR      CHKNUM
067700030822     C                   IF        PiNum=*on
067800030822     C                   Z-ADD     PiVal         VABQFT
067900030822     C                   ENDIF
068000030822     C                   ENDIF
068100030822     C* CBO
068200030822     C                   IF        %subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):1:3)
068500030822     C                             = 'CBO'
068600030822     C                   EVAL      VABCBO=%trim(%subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900030822     C                   ENDIF
069000030822     C* TSP
069100030822     C                   IF        %subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):1:3)
069400030822     C                             = 'TSP'
069500030822     C                   EVAL      VABTSP=%trim(%subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069800030822     C                   ENDIF
069900030822     C* VAS
070000030822     C                   IF        %subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):1:3)
070300030822     C                             = 'VAS'
070400030822     C                   EVAL      VABVAS=%trim(%subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700030822     C                   ENDIF
070800030822     C* VCA
070900030822     C                   IF        %subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):1:3)
071200030822     C                             = 'VCA'
071300030822     C                   EVAL      VABVCA=%trim(%subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071600030822     C                   ENDIF
071700070925     C* TIC
071800070925     C                   IF        %subst(
071900070925     C                             %subst(vlrppt:posDaDft+1:
072000070925     C                             posADft-posDaDft-1):1:3)
072100070925     C                             = 'TIC'
072200070925     C                   EVAL      VABTIC=%trim(%subst(
072300070925     C                             %subst(vlrppt:posDaDft+1:
072400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072500070925     C                   ENDIF
072600030822     C* GCA
072700030822     C                   IF        %subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):1:3)
073000030822     C                             = 'GCA'
073100030822     C                   EVAL      VABGCA=%trim(%subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073400030822     C                   ENDIF
073500030822     C* CTM
073600030822     C                   IF        %subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):1:3)
073900030822     C                             = 'CTM'
074000030822     C                   EVAL      VABCTM=%trim(%subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074300030822     C                   ENDIF
074400030822     C* FFD
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800030822     C                             = 'FFD'
074900030822     C                   EVAL      VABFFD=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   ENDIF
075300030822     C* VAD
075400030822     C                   IF        %subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):1:3)
075700030822     C                             = 'VAD'
075800030822     C                   EVAL      VABVAD=%trim(%subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100030822     C                   ENDIF
076200030822     C* GMA
076300030822     C                   IF        %subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):1:3)
076600030822     C                             = 'GMA'
076700030822     C                   EVAL      VABGMA=%trim(%subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000030822     C                   ENDIF
077100030822     C* GGA
077200030822     C                   IF        %subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):1:3)
077500030822     C                             = 'GGA'
077600030822     C                   EVAL      VABGGA=%trim(%subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077900030822     C                   ENDIF
078000030822     C* GVA
078100030822     C                   IF        %subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):1:3)
078400030822     C                             = 'GVA'
078500030822     C                   EVAL      VABGVA=%trim(%subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078800030822     C                   ENDIF
078900030822     C* TC1
079000030822     C                   IF        %subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):1:3)
079300030822     C                             = 'TC1'
079400030822     C                   EVAL      VABTC1=%trim(%subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700030822     C                   ENDIF
079800030822     C* TC2
079900030822     C                   IF        %subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):1:3)
080200030822     C                             = 'TC2'
080300030822     C                   EVAL      VABTC2=%trim(%subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600030822     C                   ENDIF
080700040714     C* VATTRC
080800040714     C                   IF        %subst(
080900040714     C                             %subst(vlrppt:posDaDft+1:
081000040714     C                             posADft-posDaDft-1):1:3)
081100040714     C                             = 'TRC'
081200040714     C                   EVAL      VATTRC=%trim(%subst(
081300040714     C                             %subst(vlrppt:posDaDft+1:
081400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081500040714     C                   ENDIF
081600030822     C* ...
081700030822     C                   ENDIF
081800030822     C                   ENDDO
081900020204     C*
082000010330     C                   ENDSR
082100010607     C*----------------------------------------------------*
082200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
082300010607     C*----------------------------------------------------*
082400010607     C     IMPVAB        BEGSR
082500010607     C*
082600010607     C                   SETOFF                                       3132
082700110315 xxx C***                SETON                                        35
082800030515     C*
082900010607     C                   MOVEL     datcor        VABAAS
083000010607     C                   MOVE      datcor        VABMGS
083100010607     C*
083200030715     C**********
083300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083400030715     C**********
083500030715     C                   IF        wGiro = *zeros
083600040113     C*
083700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
083800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
083900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
084000080429     C                             %trim(intcol(5))<>*blanks
084100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
084200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
084300080429     C                                      %trim(intcol(5))
084400070823     C     ';':CharCSV   XLATE     vindta        vindta
084500040510     C                   ENDIF
084600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084700031201     C     minu:maiu     XLATE     vindta        vindta
084800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084900030715     C                   DOW       posDa <= %len(%trim(vindta))
085000030715     C*
085100030715     C* Gestisco il 1° campo
085200030715     C                   IF        i = 1
085300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085400030715     C                   EVAL      posDa = 1
085500030715     C* Eseguo lo scan x trovare la fine del primo campo
085600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085700030715     C* A questo punto "estraggo" il campo corrente
085800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085900030715     C                                                    (posA-posDa))
086000030715     C* X i campi successivi al 1°
086100030715     C                   ELSE
086200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086300030715     C                   EVAL      posDa = posA + 1
086400030715     C* Eseguo lo scan x trovare la fine del campo corrente
086500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086600030715     C* Gestisco l'ultimo campo
086700030715     C                   IF        posA = *zeros
086800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086900030715     C* A questo punto "estraggo" il campo corrente
087000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087100030715     C                                                    (posA-posDa))
087200030715     C                   LEAVE
087300030715     C                   ELSE
087400030715     C* A questo punto "estraggo" il campo corrente
087500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087600030715     C                                                    (posA-posDa))
087700030715     C                   ENDIF
087800030715     C                   ENDIF
087900030715     C* Incremento il contatore d campo
088000030715     C                   EVAL      i = i +1
088100030715     C                   ENDDO
088200030715     C*
088300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088400030715     C* delimitazione testo
088500030715     C                   EVAL      i = 1
088600030715     C                   DOW       i < %elem(SkSplitFLD)
088700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
088800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
088900030715     C                   EVAL      i = i + 1
089000030715     C                   ENDDO
089100030715     C                   EVAL      wGiro = 1
089200030715     C                   ELSE
089300030715     C                   EVAL      wGiro = 2
089400031204     C**********
089500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
089600031204     C* d separatore campo
089700031204     C**********
089800031204     C                   EVAL      vindta = %trim(vindta)
089900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090000031204     C                   EVAL      vindta = %subst(vindta:2)
090100031204     C                   ENDDO
090200031204     C*
090300031204     C                   Z-ADD     *zeros        lunghInput        4 0
090400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
090600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
090700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090800110912     C                   ENDDO
090900030715     C**********
091000030715     C* Effettuo lo split del campo dati d input
091100030715     C**********
091200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091300030715     C                   DOW       posDa <= %len(%trim(vindta))
091400030715     C*
091500030715     C* Gestisco il 1° campo
091600030715     C                   IF        i = 1
091700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091800030715     C                   EVAL      posDa = 1
091900030715     C* Eseguo lo scan x trovare la fine del primo campo
092000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092100030715     C* A questo punto "estraggo" il campo corrente
092200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092300030715     C                                                    (posA-posDa))
092400030715     C* X i campi successivi al 1°
092500030715     C                   ELSE
092600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092700030715     C                   EVAL      posDa = posA + 1
092800031204     C* Verifico che nn vi sia il campo nn valorizzato
092900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093000031204     C* Se campo nn valorizzato skippo al prossimo
093100031204     C                   EVAL      SkSplitCSV(i) = *blanks
093200031204     C                   EVAL      posA  = posA + 1
093300031204     C                   ELSE
093400030715     C* Eseguo lo scan x trovare la fine del campo corrente
093500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093600030715     C* Gestisco l'ultimo campo
093700030715     C                   IF        posA = *zeros
093800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093900030715     C* A questo punto "estraggo" il campo corrente
094000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094100030715     C                                                    (posA-posDa))
094200030715     C                   LEAVE
094300030715     C                   ELSE
094400030715     C* A questo punto "estraggo" il campo corrente
094500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094600030715     C                                                    (posA-posDa))
094700030715     C                   ENDIF
094800031204     C                   ENDIF
094900030715     C                   ENDIF
095000030715     C* Incremento il contatore d campo
095100030715     C                   EVAL      i = i +1
095200030715     C                   ENDDO
095300030715     C*
095400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095500030715     C* delimitazione testo
095600030715     C                   EVAL      i = 1
095700030715     C                   DOW       i < %elem(SkSplitCSV)
095800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096000030715     C                   EVAL      i = i + 1
096100030715     C                   ENDDO
096200100720     C*
096300120509     C* NON Stacco un progressivo x numero spedizione, in quanto NSP = RMN
096400120509 xxx C***                EXSR      repNSP
096500100326     C*
096600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096700030715     C                   EVAL      i = 1
096800030715     C                   DOW       i < %elem(SkSplitFLD)
096900030715     C*
097000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097200030715     C                             SkSplitCSV(i) <> *blanks
097300030721     C*** ATB
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097500030721     C                   EVAL      VABATB = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030715     C*** RSD
097800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
097900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098200030715     C* ==
098300030715     C                   ENDIF
098400030721     C*** RD2
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
098600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030715     C*** IND
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099000030715     C                   EVAL      VABIND = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030715     C*** LOD
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030715     C*** PRD
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030721     C*** NZD
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030715     C*** RMA
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
100600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** NOT
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** NT2
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030715     C*** VCA
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101900030715     C                   ENDIF
102000030715     C*** VAS
102100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102300030715     C                   ENDIF
102400030715     C*** FFD
102500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
102600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
102700030715     C                   ENDIF
102800030721     C*** GC1
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** GC2
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** TSP
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
103900030721     C                   ENDIF
104000040510     C*** CBO
104100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
104300040510     C                   ENDIF
104400030721     C*** NAS
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
104600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** TIC
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** GCA
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
105400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** XCO
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** CTM
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
106200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** TCR
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
106600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** CTS
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** FTM
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
107400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** VAD
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** GMA
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
108200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** GGA
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
108600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** GVA
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** TC1
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
109400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** TC2
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** SCL
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
110200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** RMO
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
110600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** NMO
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*
111300030721     C* Reperisco quindi i campi numerici...
111400040510     C*** NSP
111500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
111600040510     C                   EVAL      PiStr=SkSplitCSV(i)
111700040510     C                   EXSR      CHKNUM
111800040510     C                   IF        PiInt=*on
111900040510     C                   Z-ADD     PiVal         VABNSP
112000040510     C                   ELSE
112100040510     C                   SETON                                        31
112200040510     C                   EVAL      VABNSP = *zeros
112300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
112400040510     C                             + ' ' + 'VABNSP'
112500040510     C                   ENDIF
112600040510     C                   ENDIF
112700071106     C*** RMN
112800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
112900071106     C                   EVAL      PiStr=SkSplitCSV(i)
113000071106     C                   EXSR      CHKNUM
113100071106     C                   IF        PiInt=*on
113200071106     C                   Z-ADD     PiVal         VABRMN
113300071106     C                   ELSE
113400071106     C                   SETON                                        32
113500071106     C                   EVAL      VABRMN = *zeros
113600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
113700071106     C                             + ' ' + 'VABRMN'
113800071106     C                   ENDIF
113900071106     C                   ENDIF
114000030721     C*** CAD
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiInt=*on
114500030721     C                   Z-ADD     PiVal         Num5_0
114600030721     C                   MOVEL(p)  Num5_0        VABCAD
114700030721     C                   ELSE
114800030721     C                   SETON                                        32
114900030721     C                   EVAL      VABCAD = *zeros
115000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115100030721     C                             + ' ' + 'VABCAD'
115200030721     C                   ENDIF
115300030721     C                   ENDIF
115400030721     C*** NCL
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiInt=*on
115900030721     C                   Z-ADD     PiVal         VABNCL
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      VABNCL = *zeros
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400040122     C                             + ' ' + 'VABNCL'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** PKB
116800120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
116900120418     C* per cui si arrotonda il valore passato agli etti
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiNum=*on
117400120418     C                   Z-ADD(H)  PiVal         VABPKB
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      VABPKB = *zeros
117800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117900030721     C                             + ' ' + 'VABPKB'
118000030721     C                   ENDIF
118100030721     C                   ENDIF
118200030721     C*** CAS
118300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
118500111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
118600111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
118700120227 xxx C                   EVAL      VABvca = 'EUR'
118800101018     C                   EVAL      wFlgCAS = '1'
118900101018     C                   ENDIF
119000030721     C                   EVAL      PiStr=SkSplitCSV(i)
119100030721     C                   EXSR      CHKNUM
119200030721     C                   IF        PiNum=*on
119300030721     C                   Z-ADD     PiVal         VABCAS
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      VABCAS = *zeros
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800030721     C                             + ' ' + 'VABCAS'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** CCM
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiInt=*on      AND
120600030721     C                             PiVal<=9999999 AND
120700030721     C                             PiVal>*zeros
120800030721     C                   Z-ADD     PiVal         VABCCM
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      VABCCM = *zeros
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300040510     C                             + ' ' + 'VABCCM'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** LNP
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiInt=*on    AND
122100030721     C                             PiVal<=999   AND
122200030721     C                             PiVal>*zeros
122300030721     C                   Z-ADD     PiVal         VABLNP
122400030721     C                   ELSE
122500040715     C                   SETON                                        32
122600030721     C                   EVAL      VABLNP = *zeros
122700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122800040510     C                             + ' ' + 'VABLNP'
122900030721     C                   ENDIF
123000030721     C                   ENDIF
123100030721     C*** NRS
123200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
123300030721     C                   EVAL      PiStr=SkSplitCSV(i)
123400030721     C                   EXSR      CHKNUM
123500030721     C                   IF        PiInt=*on AND
123600030721     C                             PiVal<=99
123700030721     C                   Z-ADD     PiVal         VABNRS
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100030721     C                             + ' ' + 'VABNRS'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** LNA
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiInt=*on    AND
124900030721     C                             PiVal<=999   AND
125000030721     C                             PiVal>*zeros
125100030721     C                   Z-ADD     PiVal         VABLNA
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400030721     C                   EVAL      VABLNA = *zeros
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABLNA'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** CTR
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiInt=*on  AND
126400030721     C                             PiVal<=999
126500030721     C                   Z-ADD     PiVal         VABCTR
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABCTR'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** IAS
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiNum=*on
127700030721     C                   Z-ADD     PiVal         VABIAS
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABIAS'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** VLB
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiNum=*on
128900030721     C                   Z-ADD     PiVal         VABVLB
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABVLB'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** QFT
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiNum=*on
130100030721     C                   Z-ADD     PiVal         VABQFT
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130500030721     C                             + ' ' + 'VABQFT'
130600030721     C                   ENDIF
130700030721     C                   ENDIF
130800030721     C*** NCD
130900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
131000030721     C                   EVAL      PiStr=SkSplitCSV(i)
131100030721     C                   EXSR      CHKNUM
131200030721     C                   IF        PiInt=*on      AND
131300030721     C                             PiVal<=9999999
131400030721     C                   Z-ADD     PiVal         VABNCD
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABNCD'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100030721     C*** NCA
132200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiInt=*on      AND
132600030721     C                             PiVal<=9999999
132700030721     C                   Z-ADD     PiVal         VABNCA
132800030721     C                   ELSE
132900030721     C                   SETON                                        32
133000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133100030721     C                             + ' ' + 'VABNCA'
133200030721     C                   ENDIF
133300030721     C                   ENDIF
133400030721     C*** ZNC
133500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133600030721     C                   EVAL      PiStr=SkSplitCSV(i)
133700030721     C                   EXSR      CHKNUM
133800030721     C                   IF        PiInt=*on AND
133900030721     C                             PiVal<=99
134000030721     C                   Z-ADD     PiVal         VABZNC
134100030721     C                   ELSE
134200030721     C                   SETON                                        32
134300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134400030721     C                             + ' ' + 'VABZNC'
134500030721     C                   ENDIF
134600030721     C                   ENDIF
134700030721     C*** DCR
134800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134900030721     C                   EVAL      PiStr=SkSplitCSV(i)
135000030721     C                   EXSR      CHKNUM
135100030721     C                   IF        PiInt=*on       AND
135200030721     C                             PiVal<=99999999
135300030721     C                   Z-ADD     PiVal         VABDCR
135400030721     C                   ELSE
135500030721     C                   SETON                                        32
135600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135700030721     C                             + ' ' + 'VABDCR'
135800030721     C                   ENDIF
135900030721     C                   ENDIF
136000030721     C*** HCR
136100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136200030721     C                   EVAL      PiStr=SkSplitCSV(i)
136300030721     C                   EXSR      CHKNUM
136400030721     C                   IF        PiInt=*on   AND
136500030721     C                             PiVal<=9999
136600030721     C                   Z-ADD     PiVal         VABHCR
136700030721     C                   ELSE
136800030721     C                   SETON                                        32
136900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137000030721     C                             + ' ' + 'VABHCR'
137100030721     C                   ENDIF
137200030721     C                   ENDIF
137300030721     C*** VMD
137400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137500030721     C                   EVAL      PiStr=SkSplitCSV(i)
137600030721     C                   EXSR      CHKNUM
137700030721     C                   IF        PiNum=*on
137800030721     C                   Z-ADD     PiVal         VABVMD
137900030721     C                   ELSE
138000030721     C                   SETON                                        32
138100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138200030721     C                             + ' ' + 'VABVMD'
138300030721     C                   ENDIF
138400030721     C                   ENDIF
138500030721     C*** ANT
138600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138700030721     C                   EVAL      PiStr=SkSplitCSV(i)
138800030721     C                   EXSR      CHKNUM
138900030721     C                   IF        PiInt=*on        AND
139000030721     C                             PiVal<=999999999
139100030721     C                   Z-ADD     PiVal         VABANT
139200030721     C                   ELSE
139300030721     C                   SETON                                        32
139400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139500030721     C                             + ' ' + 'VABANT'
139600030721     C                   ENDIF
139700030721     C                   ENDIF
139800030721     C*** CMO
139900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
140000030721     C                   EVAL      PiStr=SkSplitCSV(i)
140100030721     C                   EXSR      CHKNUM
140200030721     C                   IF        PiInt=*on
140300030721     C                   Z-ADD     PiVal         Num5_0
140400030721     C                   MOVEL(p)  Num5_0        VABCMO
140500030721     C                   ELSE
140600030721     C                   SETON                                        32
140700030721     C                   EVAL      VABCMO = *zeros
140800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140900030721     C                             + ' ' + 'VABCMO'
141000030721     C                   ENDIF
141100030721     C                   ENDIF
141200040510     C*
141300040510     C* Reperisco i campi del FIVAT
141400040510     C*** VATNOT
141500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141700040510     C                   ENDIF
141800040510     C*** VATTRC
141900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
142000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
142100040510     C                   ENDIF
142200060331     C*** VATNOT_A
142300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142500060331     C                   ENDIF
142600060331     C*** VATNOT_B
142700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142900060331     C                   ENDIF
143000060331     C*** VATNOT_E
143100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143300060331     C                   ENDIF
143400110927     C*** VATNOT_I
143500110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
143600110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
143700110927     C                   ENDIF
143800170920     C*** VATNOT_S
143900170920     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
144000170920     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
144100170920     C                   ENDIF
144200070823     C*
144300100831     C***  ===>  Gestione campi "particolari" dentro ciclo
144400120301
144500120524     C* NSP viene valorizzato con RMN
144600120524     C                   EVAL      VABNSP = VABRMN
144700120515     C*
144800120524     C* VATNOT_B viene valorizzato col concat tra W_VATNOTB1 e B2
144900120524     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTB1'
145000120524     C                   EVAL      PrefixTel=%trim(SkSplitCSV(i))
145100120515     C                   ENDIF
145200120524     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTB2'
145300120524     C                   EVAL      Telefono=%trim(SkSplitCSV(i))
145400120524     C                   ENDIF
145500120509
145600070925     C*
145700071029     C***  <===  -----------------------------------------
145800040510     C*
145900030715     C                   ENDIF
146000030715     C                   EVAL      i = i + 1
146100030715     C                   ENDDO
146200070925     C*
146300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
146400120411     C*
146500120509
146600120524     C*
146700120524     C* VATNOT_B viene valorizzato col concat tra W_VATNOTB1 e B2
146800120524     C                   EVAL      wVATNOT_B = %trim(PrefixTel) +
146900120524     C                                         %trim(Telefono)
147000170920     C*
147100170920     C* VATNOT_S viene valorizzato come VATNOT_B
147200170920     C                   EVAL      wVATNOT_S = wVATNOT_B
147300120524     C*
147400120524     C* VATNOT_E viene valorizzato inizialmente con nr. fisso 332 + l'ordine AVON = RMN = NSP
147500120524     C* poi la parte sequenziale la sviluppo successivamente rispetto al nr.colli
147600120524     C                   EVAL      wVATNOT_E = '332' +
147700120524     C                                %editc(VABNSP:'X')
147800120509
147900070925     C*
148000071029     C***  <===  -----------------------------------------
148100070925     C*
148200070925     C* Considerazioni finali su CBO/CAS
148300100318     C                   IF        wFlgCAS = '1'    OR
148400100318     C                             VABCAS  > *zeros
148500070925     C                   IF        VABCBO = '1'
148600070925     C                   EVAL      VABCBO = '4'
148700070925     C                   ENDIF
148800070925     C                   IF        VABCBO = '2'
148900070925     C                   EVAL      VABCBO = '6'
149000070925     C                   ENDIF
149100070925     C                   ENDIF
149200070925     C***  <===  ----------------------------
149300040510     C*
149400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149500040510     C                   IF        VABNSP = *zeros
149600040510     C                   SETON                                        31
149700040510     C                   ENDIF
149800040715     C*
149900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
150000040715     C                   IF        VABLNP = *zeros
150100040715     C                   SETON                                        31
150200040715     C                   ENDIF
150300040510     C*
150400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150500120411     C                   IF        VABRMN = *zeros
150600120411     C                   Z-ADD     VABNSP        VABRMN
150700120411     C                   ENDIF
150800120328     C* IN QUESTO CASO LO FORZO = 1
150900120411     C***                IF        VABRMN = *zeros
151000120411     C***                Z-ADD     1             VABRMN
151100120411     C***                ENDIF
151200040510     C*
151300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
151400040510     C                   IF        VABRMA = *blanks
151500040714     C                   MOVEL     VABRMN        VABRMA
151600040510     C                   ENDIF
151700040128     C*
151800040510     C* Se provincia nn valorizzata la reperisco
151900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
152000110912     C                   IF        VABPRD =  *blanks AND
152100110912     C                             VABNZD =  *blanks AND
152200110912     C                             VABCAD <> *blanks AND
152300110912     C                             VABLOD <> *blanks
152400040128     C                   CLEAR                   TISI95DS
152500040128     C                   EVAL      I95TCN = '3'
152600040128     C                   Z-ADD     datcor        I95DAT
152700040128     C                   EVAL      I95NAR = VABNZD
152800040128     C                   EVAL      I95CAP = VABCAD
152900040128     C                   EVAL      I95LOC = VABLOD
153000040128     C                   CALL      'TISI95R'
153100040128     C                   PARM                    TISI95DS
153200040128     C                   EVAL      VABPRD = O95PRV
153300040510     C                   ENDIF
153400020204     C*
153500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153600020204     C                   EXSR      CHKIMPDIV
153700030715     C                   ENDIF
153800020204     C*
153900010607     C                   ENDSR
154000010607     C*----------------------------------------------------*
154100010601
154200020204
154300020204     C*----------------------------------------------------*
154400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154500020204     C*----------------------------------------------------*
154600020204     C     CHKIMPDIV     BEGSR
154700020204     C*
154800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154900020204     C                   Z-ADD     *zeros        wrkDec            9 9
155000020204     C*
155100020204     C* Come prima cosa effettuo considerazioni sulla divisa
155200020204     C                   IF        vabIAS > *zeros
155300020204     C                   IF        vabVAS <> 'EUR'
155400020204     C                   EVAL      vabVAS =  'ITL'
155500020204     C                   ENDIF
155600020204     C                   ENDIF
155700020204     C*
155800020204     C                   IF        vabCAS > *zeros
155900020204     C                   IF        vabVCA <> 'EUR'
156000020204     C                   EVAL      vabVCA =  'ITL'
156100020204     C                   ENDIF
156200020204     C                   ENDIF
156300020204     C*
156400020204     C                   IF        vabVMD > *zeros
156500020204     C                   IF        vabVAD <> 'EUR'
156600020204     C                   EVAL      vabVAD =  'ITL'
156700020204     C                   ENDIF
156800020204     C                   ENDIF
156900020204     C*
157000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
157100020204     C                   Z-ADD     vabIAS        wrkDec
157200020204     C                   IF        wrkDec > *zeros
157300020204     C                   IF        vabVAS = 'ITL'
157400020204     C                   EVAL      vabIAS = *zeros
157500020204     C                   ENDIF
157600020204     C                   ENDIF
157700020204     C*
157800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157900020204     C                   Z-ADD     vabCAS        wrkDec
158000020204     C                   IF        wrkDec > *zeros
158100020204     C                   IF        vabVCA = 'ITL'
158200020204     C                   EVAL      vabCAS = *zeros
158300020204     C                   ENDIF
158400020204     C                   ENDIF
158500020204     C*
158600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158700020204     C                   Z-ADD     vabVMD        wrkDec
158800020204     C                   IF        wrkDec > *zeros
158900020204     C                   IF        vabVAD = 'ITL'
159000020204     C                   EVAL      vabVMD = *zeros
159100020204     C                   ENDIF
159200020204     C                   ENDIF
159300020204     C*
159400020204     C                   ENDSR
159500020204     C***
159600010330
159700100318
159800100318
159900100318     C*----------------------------------------------------*
160000100318     C*  CONTROLLO NUMERICITA' CAMPI
160100100318     C*----------------------------------------------------*
160200100318     C     CHKNUM        BEGSR
160300100318     C*
160400100318     C                   IF        PiDecChr = *blanks
160500100318     C                   EVAL      PiDecChr = CharNUM
160600100318     C                   ENDIF
160700100318     C*
160800100318     C                   callp     UBISNUM_Check(PiStr
160900100318     C                                          :PiDecChr
161000100318     C                                          :PiVal
161100100318     C                                          :PiNum
161200100318     C                                          :PiInt)
161300100318     C*
161400100318     C                   ENDSR
161500100318     C***
161600010601
161700010601
161800010601
161900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
162000010601     C     repfil        BEGSR
162100010601     C*
162200010601     C                   if        invfil = *zeros and
162300010601     C                             depfil > *zeros and
162400010629     C                             (vinflg = *blanks or
162500010629     C                              vinflg = *zeros)
162600010601     C
162700010601     C                   eval      invfil = depfil
162800010601     C                   endif
162900010601     C*
163000010601     C                   if        depfil <> invfil and
163100010601     C                             invfil > *zeros
163200010601     C                   eval      flgMulti = '1'
163300010601     C                   if        vinflg = *blanks
163400010601     C                   add       1             cntNonEl
163500010601     C                   endif
163600010601     C                   endif
163700010601     C*
163800010601     C                   if        vinflg = '2'
163900010601     C                   eval      flgStato = '2'
164000010601     C                   endif
164100010601     C*
164200010601     C                   ENDSR
164300010601     C***
164400010601
164500010601
164600010601
164700010330
164800040119      /TITLE Invio dei dati al punto operativo.
164900040119     C     invio         BEGSR
165000040119     C*
165100040119     C* 1° invio FIVAT
165200040119     C                   reset                   dscmz
165300040119     C                   move      vlrpoi        cmzdst
165400040119     C                   eval      cmzfld = 'FIVATWWR'
165500040119     C                   eval      cmzmbd = vlrhdl
165600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165700040119     C***                if        prmfir = *blanks
165800040119     C                   eval      cmzfla = 'FIVAT00F'
165900040119     C                   eval      cmzmba = 'FIVAT00F'
166000040119     C***                else
166100040119     C***                eval      cmzfla = prmfir
166200040119     C***                eval      cmzmba = prmfir
166300040119     C***                endif
166400040119     C                   eval      cmznrr = *zeros
166500040119     C                   move      §ctrokvt      cmznrr
166600040119     C                   eval      cmzlba = vlrfl1
166700040119     C                   call(e)   'TIS711C'
166800040119     C                   parm                    dscmz
166900040119     C                   parm      *blanks       esito
167000040119     C                   if        %error
167100040119     C                             or cmzerr = '1'
167200040119     C                             or esito  = '1'
167300040119     C                   eval      wrkesito = '3'
167400040119     C                   else
167500040119     C*
167600040119     C* 2° invio FIVAB
167700040119     C                   reset                   dscmz
167800040119     C                   move      vlrpoi        cmzdst
167900040119     C                   eval      cmzfld = vlrfou
168000040119     C                   eval      cmzmbd = vlrhdl
168100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168200040119     C***                if        prmfir = *blanks
168300040119     C                   eval      cmzfla = 'FIVAB00F'
168400040119     C                   eval      cmzmba = 'FIVAB00F'
168500040119     C***                else
168600040119     C***                eval      cmzfla = prmfir
168700040119     C***                eval      cmzmba = prmfir
168800040119     C***                endif
168900040119     C                   eval      cmznrr = *zeros
169000040119     C                   move      §ctrokvb      cmznrr
169100040119     C                   eval      cmzlba = vlrfl1
169200040119     C                   call(e)   'TIS711C'
169300040119     C                   parm                    dscmz
169400040119     C                   parm      *blanks       esito
169500040119     C                   if        %error
169600040119     C                             or cmzerr = '1'
169700040119     C                             or esito  = '1'
169800040119     C                   eval      wrkesito = '3'
169900040119     C                   endif
170000040119     C                   endif
170100040119     C*
170200040119     C                   ENDSR
170300040119     C***
170400010601
170500010601
170600010601
170700010601
170800010601      /TITLE Invio dei dati al punto operativo.
170900010601     C     opeini        BEGSR
171000010601     C*
171100010601     C* Inizializzo flag e contatori operativi
171200010601     C                   movel     '0'           flgGiro           1
171300010601     C                   movel     '0'           flgMulti          1
171400010601     C                   movel     '1'           flgStato          1
171500010615     C                   movel     '0'           flgOk             1
171600010601     C                   z-add     *zeros        cntNonEl         10 0
171700010601     C                   z-add     *zeros        depfil            3 0
171800010601     C                   z-add     *zeros        invfil            3 0
171900010601     C*
172000010601     C                   ENDSR
172100010601     C***
172200070326
172300070326
172400070326
172500070326
172600070326     C     *pssr         BEGSR
172700070329     C*
172800070329     C                   if        %open(tivin00r)
172900070329     C                   close     tivin00r
173000070329     C                   endif
173100070329     C                   if        %open(fivabwwr)
173200070329     C                   close     fivabwwr
173300070329     C                   endif
173400070329     C                   if        %open(fivatwwr)
173500070329     C                   close     fivatwwr
173600070329     C                   endif
173700070326     C*
173800070326     C* Effettuo la chiamata al CLLE preposto
173900070326     C                   call(e)   'TITVVTC'
174000070326     C                   parm                    parccm
174100070326     C                   parm                    parmbr
174200070326     C                   parm      '2'           paropz
174300070326     C*
174400070326     C                   eval      wrkesito = '2'
174500070404     C*
174600070404     C                   seton                                        LR
174700070326     C*
174800070326     C                   ENDSR     '*CANCL'
174900070326     C***
175000100318
175100100318
175200100318
175300100318     C     normCSV       BEGSR
175400100318     C*
175500100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
175600100318     C* separatori campi
175700100318     C                   if        CharTXT <> *blanks
175800100318     C                   z-add     *zeros        wLenVINDTA        4 0
175900100318     C                   z-add     *zeros        wFlgVINDTA        1 0
176000100318     C                   setoff                                       80
176100100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
176200100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
176300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
176400100318     C                   if        not *in80
176500100318     C                   seton                                        80
176600100318     C                   else
176700100318     C                   setoff                                       80
176800100318     C                   endif
176900100318     C                   endif
177000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
177100100318     C                             and *in80 = *on
177200100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
177300100318     C                   endif
177400100318     C                   enddo
177500100318     C                   endif
177600100318     C*
177700100318     C                   ENDSR
177800100318     C***
177900070326
178000100326
178100100326
178200100326     C*----------------------------------------------------*
178300100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
178400100326     C*----------------------------------------------------*
178500100326     C     REPNSP        BEGSR
178600100326     C*
178700100326     C* NSP => Stacco un numeratore da AZNUM
178800100326     C                   clear                   TRUL33DS
178900100326     C                   eval      I33OPE = *zeros
179000100326     C                   eval      I33CNU = 302
179100100326     C                   eval      I33NUM = 1
179200100326     C                   movel     TRUL33DS      KPJBU
179300100326     C                   call      'TRUL33R'
179400100326     C                   parm                    KPJBA
179500100326     C                   movel     KPJBU         TRUL33DS
179600100326     C                   if        O33ERR = *zeros
179700100326     C                   z-add     O33NRF        VABNSP
179800100326     C                   z-add     O33NRF        VATNSP
179900100326     C                   else
180000100326     C                   SETON                                        31
180100100326     C                   EVAL      vinmsg = %trimr(vinmsg)
180200100326     C                             + ' ' + 'VABNSP VATNSP'
180300100326     C                   endif
180400100326     C*
180500100326     C                   ENDSR
180600010330
180700100326
180800010330
180900000613     C     *inzsr        BEGSR
181000990910     C*
181100990910     C     *entry        plist
181200990920     C                   parm                    tivlrds
181300990921     C                   parm      wrkesito      esito
181400000724     C                   parm                    prmlit
181500000710     C                   parm                    prmfir
181600010330     C*
181700010330     C* CALCOLA LA DATA CORRENTE
181800100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181900100318     C                   eval      datcor = %dec(%date() : *ISO)
182000000613     C*
182100000613     C                   ENDSR
182200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182300161201VABRMN;X;X;X;X;X;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;X;W_VATNOTB1;W_VATNOTB2;VABNCL;X;X;VABPKB;X;
182400161201X;VATNOT_I
182500120418
182600111116
182700100318
