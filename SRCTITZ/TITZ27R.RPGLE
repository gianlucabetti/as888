000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120509      *
000300120509      * PARTICOLARITA':
000400120509      * Il file è una riga per collo occorre accorpare per lo stesso riferimento (colonna
000500120509      * Liefernummer = RMN) e sommare pesi e volumi:
000600120509      * a standard si rompe per NSP, per ottenere lo stesso effetto impostiamo NSP = RMN.
000700120509      * Non si sviluppano i CHI SONO perché vengono passati sul .csv.
000800120509      * Le colonne dei pesi e dei volumi non hanno sempre lo stesso separatore di decimali.
000900120509      * Il POC ha chiesto al cliente di far impostare sempre lo stesso separatore ma non sarà una
001000120509      * operazione a breve.
001100120509      * In realtà il separatore decimale sembra essere sempre il punto (.).
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
004700120509     D Intestazioni    s               n   inz(*on)
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
007400120411     D wNOT            s             70
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
021900070925     C                   eval      vindta  = ' ' + %trim(vindta)
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
032800120509     C     CHKVAB        BEGSR
032900110201     C*
033000110201     C* Se richiesti ignorare colli / peso / volume
033100110315 xxx C***                if        *in35
033200110315 xxx C***                z-add     *zeros        VABNCL
033300110315 xxx C***                z-add     *zeros        VABPKB
033400110315 xxx C***                z-add     *zeros        VABVLB
033500110315 xxx C***                endif
033600080527     C*
033700120509     C* Scrivo il VAB solo a rottura d spedizione
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
037200070301     C                   eval      VABCTM = '7Q'
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
041800060331     C                   if        wVATNOT_E <> *blanks
041900110201     C                   eval      VATNOT = wVATNOT_E
042000060331     C                   eval      VATTRC = 'E'
042100120411     C*
042200120509     C* Non devo sviluppare i "CHI SONO" perché sono scritti sul .csv
042300120411     C*
042400120410     C                   write     FIVAT000
042500120410     C                   add       1             §CTROKVT
042600120320     C                   endif
042700120320     C*
042800110927     C*
042900110927     C                   if        wVATNOT_I <> *blanks
043000110927     C                   eval      VATNOT = wVATNOT_I
043100110927     C                   eval      VATTRC = 'I'
043200110927     C                   write     FIVAT000
043300110927     C                   add       1             §CTROKVT
043400110927     C                   endif
043500040119     C*
043600040119     C                   ENDSR
043700040119
043800040119
043900040119
044000040119     C*----------------------------------------------------*
044100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044200040119     C*----------------------------------------------------*
044300040119     C     PREVAT        BEGSR
044400040119     C*
044500040119     C* Compongo il nome del membro da dare al FIVATWWR
044600040119     C                   eval      parmbr = vlrhdl
044700040119     C                   movel     'M'           parmbr
044800040510     C                   eval      parccm = vlrksc
044900040119     C                   eval      paropz = '1'
045000040119     C* Effettuo la chiamata al CLLE preposto
045100040119     C                   call(e)   'TITVVTC'
045200040119     C                   parm                    parccm
045300040119     C                   parm                    parmbr
045400040119     C                   parm                    paropz
045500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045600040119     C                   if        %error
045700040119     C                   movel     '1'           chkcall
045800040119     C                   else
045900040119     C                   movel     '0'           chkcall
046000040119     C                   endif
046100040119     C*
046200040119     C                   ENDSR
046300010601
046400010601
046500010601
046600010330     C*----------------------------------------------------*
046700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046800010330     C*----------------------------------------------------*
046900040119     C     INZVAR        BEGSR
047000080527     C*
047100080527     C                   setoff                                       65
047200010330     C*
047300030822     C* Inizializzo variabili di wrk
047400020204     C                   Z-ADD     *zeros        Num5_0            5 0
047500101018     C                   MOVEL     *blanks       wFlgCAS
047600060331     C                   MOVEL     *blanks       wVATNOT_A
047700060331     C                   MOVEL     *blanks       wVATNOT_B
047800060331     C                   MOVEL     *blanks       wVATNOT_E
047900110927     C                   MOVEL     *blanks       wVATNOT_I
048000120411     C                   MOVEL     *blanks       wNOT
048100120411     C                   MOVEL     *blanks       Data8A
048200030822     C*
048300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048400030822     C                   CLEAR                   SkSplitCSV
048500030822     C                   EVAL      i = 1
048600030822     C                   EVAL      posDa = *zeros
048700030822     C                   EVAL      posA  = *zeros
048800080527     C*
048900080527     C                   CLEAR                   FIVABDS
049000080527     C                   CLEAR                   FIVAT000
049100030822     C*
049200030822     C* Reimposto i valori di default
049300030822     C                   EXSR      DEFCAM
049400030822     C*
049500010330     C                   ENDSR
049600010330     C*----------------------------------------------------*
049700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049800010330     C*----------------------------------------------------*
049900010330     C     DEFCAM        BEGSR
050000030822     C*
050100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050200030715     C* e delimitatore testo.
050300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050600100318     C*
050700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050800100318     C                   EVAL      CharSOS = CharNUM
050900030822     C*
051000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051100100720     C                   SETOFF                                       505152
051200100720     C                   SETOFF                                       53
051300100413     C                   SELECT
051400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051500100720     C                   SETON                                        50
051600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051700100413     C                   SETON                                        51
051800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051900100720     C                   SETON                                        52
052000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
052100100720     C                   SETON                                        53
052200100413     C                   ENDSL
052300030822     C*
052400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052500030822     C                   EVAL      posDaDft = 1
052600030822     C                   EVAL      posADft  = 0
052700030822     C                   EVAL      wGiroDft = 0
052800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052900030822     C                             posDaDft > 0
053000030822     C*
053100030822     C* Gestisco il 1° giro
053200030822     C                   IF        wGiroDft = 0
053300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053500030822     C* Incremento il contatore dei "giri"
053600030822     C                   EVAL      wGiroDft = 1
053700030822     C                   ELSE
053800030822     C                   EVAL      posDaDft = posADft
053900030822     C                   ENDIF
054000030822     C* Eseguo lo scan x trovare la fine del campo corrente
054100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054200030822     C*
054300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054500030822     C                   IF        posDaDft > 0 AND
054600030822     C                             posADft  > 0
054700030822     C* NCL
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'NCL'
055200030822     C                   EVAL      PiStr=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   EXSR      CHKNUM
055600030822     C                   IF        PiInt=*on
055700030822     C                   Z-ADD     PiVal         VABNCL
055800030822     C                   ENDIF
055900030822     C                   ENDIF
056000030822     C* CCM
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'CCM'
056500030822     C                   EVAL      PiStr=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   EXSR      CHKNUM
056900030822     C                   IF        PiInt=*on
057000030822     C                   Z-ADD     PiVal         VABCCM
057100030822     C                   ENDIF
057200030822     C                   ENDIF
057300030822     C* LNP
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'LNP'
057800030822     C                   EVAL      PiStr=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   EXSR      CHKNUM
058200030822     C                   IF        PiInt=*on
058300030822     C                   Z-ADD     PiVal         VABLNP
058400030822     C                   ENDIF
058500030822     C                   ENDIF
058600030822     C* NRS
058700030822     C                   IF        %subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):1:3)
059000030822     C                             = 'NRS'
059100030822     C                   EVAL      PiStr=%trim(%subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400030822     C                   EXSR      CHKNUM
059500030822     C                   IF        PiInt=*on
059600030822     C                   Z-ADD     PiVal         VABNRS
059700030822     C                   ENDIF
059800030822     C                   ENDIF
059900030822     C* CTR
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'CTR'
060400030822     C                   EVAL      PiStr=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   EXSR      CHKNUM
060800030822     C                   IF        PiInt=*on
060900030822     C                   Z-ADD     PiVal         VABCTR
061000030822     C                   ENDIF
061100030822     C                   ENDIF
061200030822     C* PKB
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'PKB'
061700030822     C                   EVAL      PiStr=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   EXSR      CHKNUM
062100030822     C                   IF        PiNum=*on
062200120418     C                   Z-ADD     PiVal         VABPKB
062300030822     C                   ENDIF
062400030822     C                   ENDIF
062500030822     C* VLB
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'VLB'
063000030822     C                   EVAL      PiStr=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   EXSR      CHKNUM
063400030822     C                   IF        PiNum=*on
063500030822     C                   Z-ADD     PiVal         VABVLB
063600030822     C                   ENDIF
063700030822     C                   ENDIF
063800030822     C* QFT
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'QFT'
064300030822     C                   EVAL      PiStr=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   EXSR      CHKNUM
064700030822     C                   IF        PiNum=*on
064800030822     C                   Z-ADD     PiVal         VABQFT
064900030822     C                   ENDIF
065000030822     C                   ENDIF
065100030822     C* CBO
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'CBO'
065600030822     C                   EVAL      VABCBO=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* TSP
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'TSP'
066500030822     C                   EVAL      VABTSP=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* VAS
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'VAS'
067400030822     C                   EVAL      VABVAS=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* VCA
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'VCA'
068300030822     C                   EVAL      VABVCA=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700070925     C* TIC
068800070925     C                   IF        %subst(
068900070925     C                             %subst(vlrppt:posDaDft+1:
069000070925     C                             posADft-posDaDft-1):1:3)
069100070925     C                             = 'TIC'
069200070925     C                   EVAL      VABTIC=%trim(%subst(
069300070925     C                             %subst(vlrppt:posDaDft+1:
069400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500070925     C                   ENDIF
069600030822     C* GCA
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'GCA'
070100030822     C                   EVAL      VABGCA=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* CTM
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'CTM'
071000030822     C                   EVAL      VABCTM=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* FFD
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'FFD'
071900030822     C                   EVAL      VABFFD=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* VAD
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'VAD'
072800030822     C                   EVAL      VABVAD=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* GMA
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'GMA'
073700030822     C                   EVAL      VABGMA=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* GGA
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'GGA'
074600030822     C                   EVAL      VABGGA=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* GVA
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'GVA'
075500030822     C                   EVAL      VABGVA=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* TC1
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'TC1'
076400030822     C                   EVAL      VABTC1=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* TC2
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'TC2'
077300030822     C                   EVAL      VABTC2=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700040714     C* VATTRC
077800040714     C                   IF        %subst(
077900040714     C                             %subst(vlrppt:posDaDft+1:
078000040714     C                             posADft-posDaDft-1):1:3)
078100040714     C                             = 'TRC'
078200040714     C                   EVAL      VATTRC=%trim(%subst(
078300040714     C                             %subst(vlrppt:posDaDft+1:
078400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500040714     C                   ENDIF
078600030822     C* ...
078700030822     C                   ENDIF
078800030822     C                   ENDDO
078900020204     C*
079000010330     C                   ENDSR
079100010607     C*----------------------------------------------------*
079200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079300010607     C*----------------------------------------------------*
079400010607     C     IMPVAB        BEGSR
079500010607     C*
079600010607     C                   SETOFF                                       3132
079700110315 xxx C***                SETON                                        35
079800030515     C*
079900010607     C                   MOVEL     datcor        VABAAS
080000010607     C                   MOVE      datcor        VABMGS
080100010607     C*
080200030715     C**********
080300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080400030715     C**********
080500030715     C                   IF        wGiro = *zeros
080600040113     C*
080700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081000080429     C                             %trim(intcol(5))<>*blanks
081100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081300080429     C                                      %trim(intcol(5))
081400070823     C     ';':CharCSV   XLATE     vindta        vindta
081500040510     C                   ENDIF
081600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081700031201     C     minu:maiu     XLATE     vindta        vindta
081800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081900030715     C                   DOW       posDa <= %len(%trim(vindta))
082000030715     C*
082100030715     C* Gestisco il 1° campo
082200030715     C                   IF        i = 1
082300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082400030715     C                   EVAL      posDa = 1
082500030715     C* Eseguo lo scan x trovare la fine del primo campo
082600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082700030715     C* A questo punto "estraggo" il campo corrente
082800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082900030715     C                                                    (posA-posDa))
083000030715     C* X i campi successivi al 1°
083100030715     C                   ELSE
083200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083300030715     C                   EVAL      posDa = posA + 1
083400030715     C* Eseguo lo scan x trovare la fine del campo corrente
083500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083600030715     C* Gestisco l'ultimo campo
083700030715     C                   IF        posA = *zeros
083800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083900030715     C* A questo punto "estraggo" il campo corrente
084000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084100030715     C                                                    (posA-posDa))
084200030715     C                   LEAVE
084300030715     C                   ELSE
084400030715     C* A questo punto "estraggo" il campo corrente
084500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084600030715     C                                                    (posA-posDa))
084700030715     C                   ENDIF
084800030715     C                   ENDIF
084900030715     C* Incremento il contatore d campo
085000030715     C                   EVAL      i = i +1
085100030715     C                   ENDDO
085200030715     C*
085300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085400030715     C* delimitazione testo
085500030715     C                   EVAL      i = 1
085600030715     C                   DOW       i < %elem(SkSplitFLD)
085700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085900030715     C                   EVAL      i = i + 1
086000030715     C                   ENDDO
086100030715     C                   EVAL      wGiro = 1
086200030715     C                   ELSE
086300030715     C                   EVAL      wGiro = 2
086400031204     C**********
086500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086600031204     C* d separatore campo
086700031204     C**********
086800031204     C                   EVAL      vindta = %trim(vindta)
086900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087000031204     C                   EVAL      vindta = %subst(vindta:2)
087100031204     C                   ENDDO
087200031204     C*
087300031204     C                   Z-ADD     *zeros        lunghInput        4 0
087400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087800110912     C                   ENDDO
087900030715     C**********
088000030715     C* Effettuo lo split del campo dati d input
088100030715     C**********
088200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088300030715     C                   DOW       posDa <= %len(%trim(vindta))
088400030715     C*
088500030715     C* Gestisco il 1° campo
088600030715     C                   IF        i = 1
088700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088800030715     C                   EVAL      posDa = 1
088900030715     C* Eseguo lo scan x trovare la fine del primo campo
089000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089100030715     C* A questo punto "estraggo" il campo corrente
089200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089300030715     C                                                    (posA-posDa))
089400030715     C* X i campi successivi al 1°
089500030715     C                   ELSE
089600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089700030715     C                   EVAL      posDa = posA + 1
089800031204     C* Verifico che nn vi sia il campo nn valorizzato
089900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090000031204     C* Se campo nn valorizzato skippo al prossimo
090100031204     C                   EVAL      SkSplitCSV(i) = *blanks
090200031204     C                   EVAL      posA  = posA + 1
090300031204     C                   ELSE
090400030715     C* Eseguo lo scan x trovare la fine del campo corrente
090500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090600030715     C* Gestisco l'ultimo campo
090700030715     C                   IF        posA = *zeros
090800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   LEAVE
091300030715     C                   ELSE
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C                   ENDIF
091800031204     C                   ENDIF
091900030715     C                   ENDIF
092000030715     C* Incremento il contatore d campo
092100030715     C                   EVAL      i = i +1
092200030715     C                   ENDDO
092300030715     C*
092400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092500030715     C* delimitazione testo
092600030715     C                   EVAL      i = 1
092700030715     C                   DOW       i < %elem(SkSplitCSV)
092800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093000030715     C                   EVAL      i = i + 1
093100030715     C                   ENDDO
093200100720     C*
093300120509     C* NON Stacco un progressivo x numero spedizione, in quanto NSP = RMN
093400120509 xxx C***                EXSR      repNSP
093500100326     C*
093600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093700030715     C                   EVAL      i = 1
093800030715     C                   DOW       i < %elem(SkSplitFLD)
093900030715     C*
094000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094200030715     C                             SkSplitCSV(i) <> *blanks
094300030721     C*** ATB
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094500030721     C                   EVAL      VABATB = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030715     C*** RSD
094800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095200030715     C* ==
095300030715     C                   ENDIF
095400030721     C*** RD2
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030715     C*** IND
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096000030715     C                   EVAL      VABIND = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030715     C*** LOD
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** PRD
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030721     C*** NZD
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030715     C*** RMA
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** NOT
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** NT2
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** VCA
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030715     C*** VAS
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099300030715     C                   ENDIF
099400030715     C*** FFD
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030721     C*** GC1
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** GC2
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** TSP
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100900030721     C                   ENDIF
101000040510     C*** CBO
101100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101300040510     C                   ENDIF
101400030721     C*** NAS
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** TIC
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** GCA
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** XCO
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** CTM
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** TCR
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** CTS
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** FTM
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** VAD
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** GMA
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** GGA
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** GVA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** TC1
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** TC2
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** SCL
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** RMO
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** NMO
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*
108300030721     C* Reperisco quindi i campi numerici...
108400040510     C*** NSP
108500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108600040510     C                   EVAL      PiStr=SkSplitCSV(i)
108700040510     C                   EXSR      CHKNUM
108800040510     C                   IF        PiInt=*on
108900040510     C                   Z-ADD     PiVal         VABNSP
109000040510     C                   ELSE
109100040510     C                   SETON                                        31
109200040510     C                   EVAL      VABNSP = *zeros
109300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109400040510     C                             + ' ' + 'VABNSP'
109500040510     C                   ENDIF
109600040510     C                   ENDIF
109700071106     C*** RMN
109800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109900071106     C                   EVAL      PiStr=SkSplitCSV(i)
110000071106     C                   EXSR      CHKNUM
110100071106     C                   IF        PiInt=*on
110200071106     C                   Z-ADD     PiVal         VABRMN
110300071106     C                   ELSE
110400071106     C                   SETON                                        32
110500071106     C                   EVAL      VABRMN = *zeros
110600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110700071106     C                             + ' ' + 'VABRMN'
110800071106     C                   ENDIF
110900071106     C                   ENDIF
111000030721     C*** CAD
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiInt=*on
111500030721     C                   Z-ADD     PiVal         Num5_0
111600030721     C                   MOVEL(p)  Num5_0        VABCAD
111700030721     C                   ELSE
111800030721     C                   SETON                                        32
111900030721     C                   EVAL      VABCAD = *zeros
112000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112100030721     C                             + ' ' + 'VABCAD'
112200030721     C                   ENDIF
112300030721     C                   ENDIF
112400030721     C*** NCL
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiInt=*on
112900030721     C                   Z-ADD     PiVal         VABNCL
113000030721     C                   ELSE
113100030721     C                   SETON                                        32
113200030721     C                   EVAL      VABNCL = *zeros
113300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113400040122     C                             + ' ' + 'VABNCL'
113500030721     C                   ENDIF
113600030721     C                   ENDIF
113700030721     C*** PKB
113800120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
113900120418     C* per cui si arrotonda il valore passato agli etti
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiNum=*on
114400120418     C                   Z-ADD(H)  PiVal         VABPKB
114500030721     C                   ELSE
114600030721     C                   SETON                                        32
114700030721     C                   EVAL      VABPKB = *zeros
114800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114900030721     C                             + ' ' + 'VABPKB'
115000030721     C                   ENDIF
115100030721     C                   ENDIF
115200030721     C*** CAS
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115500111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
115600111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
115700120227 xxx C                   EVAL      VABvca = 'EUR'
115800101018     C                   EVAL      wFlgCAS = '1'
115900101018     C                   ENDIF
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiNum=*on
116300030721     C                   Z-ADD     PiVal         VABCAS
116400030721     C                   ELSE
116500030721     C                   SETON                                        32
116600030721     C                   EVAL      VABCAS = *zeros
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800030721     C                             + ' ' + 'VABCAS'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** CCM
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on      AND
117600030721     C                             PiVal<=9999999 AND
117700030721     C                             PiVal>*zeros
117800030721     C                   Z-ADD     PiVal         VABCCM
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      VABCCM = *zeros
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300040510     C                             + ' ' + 'VABCCM'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** LNP
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiInt=*on    AND
119100030721     C                             PiVal<=999   AND
119200030721     C                             PiVal>*zeros
119300030721     C                   Z-ADD     PiVal         VABLNP
119400030721     C                   ELSE
119500040715     C                   SETON                                        32
119600030721     C                   EVAL      VABLNP = *zeros
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800040510     C                             + ' ' + 'VABLNP'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** NRS
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiInt=*on AND
120600030721     C                             PiVal<=99
120700030721     C                   Z-ADD     PiVal         VABNRS
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100030721     C                             + ' ' + 'VABNRS'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** LNA
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on    AND
121900030721     C                             PiVal<=999   AND
122000030721     C                             PiVal>*zeros
122100030721     C                   Z-ADD     PiVal         VABLNA
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      VABLNA = *zeros
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABLNA'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** CTR
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on  AND
123400030721     C                             PiVal<=999
123500030721     C                   Z-ADD     PiVal         VABCTR
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABCTR'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** IAS
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiNum=*on
124700030721     C                   Z-ADD     PiVal         VABIAS
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABIAS'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** VLB
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiNum=*on
125900030721     C                   Z-ADD     PiVal         VABVLB
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABVLB'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** QFT
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiNum=*on
127100030721     C                   Z-ADD     PiVal         VABQFT
127200030721     C                   ELSE
127300030721     C                   SETON                                        32
127400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127500030721     C                             + ' ' + 'VABQFT'
127600030721     C                   ENDIF
127700030721     C                   ENDIF
127800030721     C*** NCD
127900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128000030721     C                   EVAL      PiStr=SkSplitCSV(i)
128100030721     C                   EXSR      CHKNUM
128200030721     C                   IF        PiInt=*on      AND
128300030721     C                             PiVal<=9999999
128400030721     C                   Z-ADD     PiVal         VABNCD
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABNCD'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030721     C*** NCA
129200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129300030721     C                   EVAL      PiStr=SkSplitCSV(i)
129400030721     C                   EXSR      CHKNUM
129500030721     C                   IF        PiInt=*on      AND
129600030721     C                             PiVal<=9999999
129700030721     C                   Z-ADD     PiVal         VABNCA
129800030721     C                   ELSE
129900030721     C                   SETON                                        32
130000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130100030721     C                             + ' ' + 'VABNCA'
130200030721     C                   ENDIF
130300030721     C                   ENDIF
130400030721     C*** ZNC
130500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130600030721     C                   EVAL      PiStr=SkSplitCSV(i)
130700030721     C                   EXSR      CHKNUM
130800030721     C                   IF        PiInt=*on AND
130900030721     C                             PiVal<=99
131000030721     C                   Z-ADD     PiVal         VABZNC
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABZNC'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** DCR
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiInt=*on       AND
132200030721     C                             PiVal<=99999999
132300030721     C                   Z-ADD     PiVal         VABDCR
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABDCR'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** HCR
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on   AND
133500030721     C                             PiVal<=9999
133600030721     C                   Z-ADD     PiVal         VABHCR
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABHCR'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** VMD
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiNum=*on
134800030721     C                   Z-ADD     PiVal         VABVMD
134900030721     C                   ELSE
135000030721     C                   SETON                                        32
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200030721     C                             + ' ' + 'VABVMD'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500030721     C*** ANT
135600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135700030721     C                   EVAL      PiStr=SkSplitCSV(i)
135800030721     C                   EXSR      CHKNUM
135900030721     C                   IF        PiInt=*on        AND
136000030721     C                             PiVal<=999999999
136100030721     C                   Z-ADD     PiVal         VABANT
136200030721     C                   ELSE
136300030721     C                   SETON                                        32
136400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136500030721     C                             + ' ' + 'VABANT'
136600030721     C                   ENDIF
136700030721     C                   ENDIF
136800030721     C*** CMO
136900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137000030721     C                   EVAL      PiStr=SkSplitCSV(i)
137100030721     C                   EXSR      CHKNUM
137200030721     C                   IF        PiInt=*on
137300030721     C                   Z-ADD     PiVal         Num5_0
137400030721     C                   MOVEL(p)  Num5_0        VABCMO
137500030721     C                   ELSE
137600030721     C                   SETON                                        32
137700030721     C                   EVAL      VABCMO = *zeros
137800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137900030721     C                             + ' ' + 'VABCMO'
138000030721     C                   ENDIF
138100030721     C                   ENDIF
138200040510     C*
138300040510     C* Reperisco i campi del FIVAT
138400040510     C*** VATNOT
138500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138700040510     C                   ENDIF
138800040510     C*** VATTRC
138900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139100040510     C                   ENDIF
139200060331     C*** VATNOT_A
139300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139500060331     C                   ENDIF
139600060331     C*** VATNOT_B
139700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139900060331     C                   ENDIF
140000060331     C*** VATNOT_E
140100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140300060331     C                   ENDIF
140400110927     C*** VATNOT_I
140500110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
140600110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
140700110927     C                   ENDIF
140800070823     C*
140900100831     C***  ===>  Gestione campi "particolari" dentro ciclo
141000120301
141100120509     C* NSP viene valorizzato con RMA
141200120509     C* uso MOVE perché NSP è lungo 7 ed RMA 15 A, non mi importa se perdo alcuni dati significativi
141300120509     C* perché il nr.spedizione ha senso solo per questo invio di csv e non come dato storico
141400120509     C                   MOVE      *blanks       wAlfa15          15
141500120509     C                   EVALR     wAlfa15 = %trimr(VABRMA)
141600120509     C                   MOVE      wAlfa15       VABNSP
141700120515     C*
141800120605     C*** RMN
141900120605     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
142000120605     C                   EVAL      PiStr=SkSplitCSV(i)
142100120605     C                   EXSR      CHKNUM
142200120605     C                   IF        PiInt=*on
142300120605     C                   Z-ADD     PiVal         VABRMN
142400120605     C* VATNOT_E viene valorizzato con RMN
142500120605     C                   EVAL      wVATNOT_E=%trim(%editc(VABRMN : '4'))
142600120605     C                   ELSE
142700120605     C                   SETON                                        32
142800120605     C                   EVAL      VABRMN = *zeros
142900120605     C                   EVAL      vinmsg = %trimr(vinmsg)
143000120605     C                             + ' ' + 'VABRMN'
143100120605     C                   ENDIF
143200120605     C                   ENDIF
143300120509
143400070925     C*
143500071029     C***  <===  -----------------------------------------
143600040510     C*
143700030715     C                   ENDIF
143800030715     C                   EVAL      i = i + 1
143900030715     C                   ENDDO
144000070925     C*
144100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144200120411     C*
144300120509
144400120509     C* non ce ne è
144500120509
144600070925     C*
144700071029     C***  <===  -----------------------------------------
144800070925     C*
144900070925     C* Considerazioni finali su CBO/CAS
145000100318     C                   IF        wFlgCAS = '1'    OR
145100100318     C                             VABCAS  > *zeros
145200070925     C                   IF        VABCBO = '1'
145300070925     C                   EVAL      VABCBO = '4'
145400070925     C                   ENDIF
145500070925     C                   IF        VABCBO = '2'
145600070925     C                   EVAL      VABCBO = '6'
145700070925     C                   ENDIF
145800070925     C                   ENDIF
145900070925     C***  <===  ----------------------------
146000040510     C*
146100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
146200040510     C                   IF        VABNSP = *zeros
146300040510     C                   SETON                                        31
146400040510     C                   ENDIF
146500040715     C*
146600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146700040715     C                   IF        VABLNP = *zeros
146800040715     C                   SETON                                        31
146900040715     C                   ENDIF
147000040510     C*
147100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
147200120411     C                   IF        VABRMN = *zeros
147300120411     C                   Z-ADD     VABNSP        VABRMN
147400120411     C                   ENDIF
147500120328     C* IN QUESTO CASO LO FORZO = 1
147600120411     C***                IF        VABRMN = *zeros
147700120411     C***                Z-ADD     1             VABRMN
147800120411     C***                ENDIF
147900040510     C*
148000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
148100040510     C                   IF        VABRMA = *blanks
148200040714     C                   MOVEL     VABRMN        VABRMA
148300040510     C                   ENDIF
148400040128     C*
148500040510     C* Se provincia nn valorizzata la reperisco
148600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
148700110912     C                   IF        VABPRD =  *blanks AND
148800110912     C                             VABNZD =  *blanks AND
148900110912     C                             VABCAD <> *blanks AND
149000110912     C                             VABLOD <> *blanks
149100040128     C                   CLEAR                   TISI95DS
149200040128     C                   EVAL      I95TCN = '3'
149300040128     C                   Z-ADD     datcor        I95DAT
149400040128     C                   EVAL      I95NAR = VABNZD
149500040128     C                   EVAL      I95CAP = VABCAD
149600040128     C                   EVAL      I95LOC = VABLOD
149700040128     C                   CALL      'TISI95R'
149800040128     C                   PARM                    TISI95DS
149900040128     C                   EVAL      VABPRD = O95PRV
150000040510     C                   ENDIF
150100020204     C*
150200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
150300020204     C                   EXSR      CHKIMPDIV
150400030715     C                   ENDIF
150500020204     C*
150600010607     C                   ENDSR
150700010607     C*----------------------------------------------------*
150800010601
150900020204
151000020204     C*----------------------------------------------------*
151100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
151200020204     C*----------------------------------------------------*
151300020204     C     CHKIMPDIV     BEGSR
151400020204     C*
151500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
151600020204     C                   Z-ADD     *zeros        wrkDec            9 9
151700020204     C*
151800020204     C* Come prima cosa effettuo considerazioni sulla divisa
151900020204     C                   IF        vabIAS > *zeros
152000020204     C                   IF        vabVAS <> 'EUR'
152100020204     C                   EVAL      vabVAS =  'ITL'
152200020204     C                   ENDIF
152300020204     C                   ENDIF
152400020204     C*
152500020204     C                   IF        vabCAS > *zeros
152600020204     C                   IF        vabVCA <> 'EUR'
152700020204     C                   EVAL      vabVCA =  'ITL'
152800020204     C                   ENDIF
152900020204     C                   ENDIF
153000020204     C*
153100020204     C                   IF        vabVMD > *zeros
153200020204     C                   IF        vabVAD <> 'EUR'
153300020204     C                   EVAL      vabVAD =  'ITL'
153400020204     C                   ENDIF
153500020204     C                   ENDIF
153600020204     C*
153700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
153800020204     C                   Z-ADD     vabIAS        wrkDec
153900020204     C                   IF        wrkDec > *zeros
154000020204     C                   IF        vabVAS = 'ITL'
154100020204     C                   EVAL      vabIAS = *zeros
154200020204     C                   ENDIF
154300020204     C                   ENDIF
154400020204     C*
154500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
154600020204     C                   Z-ADD     vabCAS        wrkDec
154700020204     C                   IF        wrkDec > *zeros
154800020204     C                   IF        vabVCA = 'ITL'
154900020204     C                   EVAL      vabCAS = *zeros
155000020204     C                   ENDIF
155100020204     C                   ENDIF
155200020204     C*
155300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
155400020204     C                   Z-ADD     vabVMD        wrkDec
155500020204     C                   IF        wrkDec > *zeros
155600020204     C                   IF        vabVAD = 'ITL'
155700020204     C                   EVAL      vabVMD = *zeros
155800020204     C                   ENDIF
155900020204     C                   ENDIF
156000020204     C*
156100020204     C                   ENDSR
156200020204     C***
156300010330
156400100318
156500100318
156600100318     C*----------------------------------------------------*
156700100318     C*  CONTROLLO NUMERICITA' CAMPI
156800100318     C*----------------------------------------------------*
156900100318     C     CHKNUM        BEGSR
157000100318     C*
157100100318     C                   IF        PiDecChr = *blanks
157200100318     C                   EVAL      PiDecChr = CharNUM
157300100318     C                   ENDIF
157400100318     C*
157500100318     C                   callp     UBISNUM_Check(PiStr
157600100318     C                                          :PiDecChr
157700100318     C                                          :PiVal
157800100318     C                                          :PiNum
157900100318     C                                          :PiInt)
158000100318     C*
158100100318     C                   ENDSR
158200100318     C***
158300010601
158400010601
158500010601
158600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158700010601     C     repfil        BEGSR
158800010601     C*
158900010601     C                   if        invfil = *zeros and
159000010601     C                             depfil > *zeros and
159100010629     C                             (vinflg = *blanks or
159200010629     C                              vinflg = *zeros)
159300010601     C
159400010601     C                   eval      invfil = depfil
159500010601     C                   endif
159600010601     C*
159700010601     C                   if        depfil <> invfil and
159800010601     C                             invfil > *zeros
159900010601     C                   eval      flgMulti = '1'
160000010601     C                   if        vinflg = *blanks
160100010601     C                   add       1             cntNonEl
160200010601     C                   endif
160300010601     C                   endif
160400010601     C*
160500010601     C                   if        vinflg = '2'
160600010601     C                   eval      flgStato = '2'
160700010601     C                   endif
160800010601     C*
160900010601     C                   ENDSR
161000010601     C***
161100010601
161200010601
161300010601
161400010330
161500040119      /TITLE Invio dei dati al punto operativo.
161600040119     C     invio         BEGSR
161700040119     C*
161800040119     C* 1° invio FIVAT
161900040119     C                   reset                   dscmz
162000040119     C                   move      vlrpoi        cmzdst
162100040119     C                   eval      cmzfld = 'FIVATWWR'
162200040119     C                   eval      cmzmbd = vlrhdl
162300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162400040119     C***                if        prmfir = *blanks
162500040119     C                   eval      cmzfla = 'FIVAT00F'
162600040119     C                   eval      cmzmba = 'FIVAT00F'
162700040119     C***                else
162800040119     C***                eval      cmzfla = prmfir
162900040119     C***                eval      cmzmba = prmfir
163000040119     C***                endif
163100040119     C                   eval      cmznrr = *zeros
163200040119     C                   move      §ctrokvt      cmznrr
163300040119     C                   eval      cmzlba = vlrfl1
163400040119     C                   call(e)   'TIS711C'
163500040119     C                   parm                    dscmz
163600040119     C                   parm      *blanks       esito
163700040119     C                   if        %error
163800040119     C                             or cmzerr = '1'
163900040119     C                             or esito  = '1'
164000040119     C                   eval      wrkesito = '3'
164100040119     C                   else
164200040119     C*
164300040119     C* 2° invio FIVAB
164400040119     C                   reset                   dscmz
164500040119     C                   move      vlrpoi        cmzdst
164600040119     C                   eval      cmzfld = vlrfou
164700040119     C                   eval      cmzmbd = vlrhdl
164800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164900040119     C***                if        prmfir = *blanks
165000040119     C                   eval      cmzfla = 'FIVAB00F'
165100040119     C                   eval      cmzmba = 'FIVAB00F'
165200040119     C***                else
165300040119     C***                eval      cmzfla = prmfir
165400040119     C***                eval      cmzmba = prmfir
165500040119     C***                endif
165600040119     C                   eval      cmznrr = *zeros
165700040119     C                   move      §ctrokvb      cmznrr
165800040119     C                   eval      cmzlba = vlrfl1
165900040119     C                   call(e)   'TIS711C'
166000040119     C                   parm                    dscmz
166100040119     C                   parm      *blanks       esito
166200040119     C                   if        %error
166300040119     C                             or cmzerr = '1'
166400040119     C                             or esito  = '1'
166500040119     C                   eval      wrkesito = '3'
166600040119     C                   endif
166700040119     C                   endif
166800040119     C*
166900040119     C                   ENDSR
167000040119     C***
167100010601
167200010601
167300010601
167400010601
167500010601      /TITLE Invio dei dati al punto operativo.
167600010601     C     opeini        BEGSR
167700010601     C*
167800010601     C* Inizializzo flag e contatori operativi
167900010601     C                   movel     '0'           flgGiro           1
168000010601     C                   movel     '0'           flgMulti          1
168100010601     C                   movel     '1'           flgStato          1
168200010615     C                   movel     '0'           flgOk             1
168300010601     C                   z-add     *zeros        cntNonEl         10 0
168400010601     C                   z-add     *zeros        depfil            3 0
168500010601     C                   z-add     *zeros        invfil            3 0
168600010601     C*
168700010601     C                   ENDSR
168800010601     C***
168900070326
169000070326
169100070326
169200070326
169300070326     C     *pssr         BEGSR
169400070329     C*
169500070329     C                   if        %open(tivin00r)
169600070329     C                   close     tivin00r
169700070329     C                   endif
169800070329     C                   if        %open(fivabwwr)
169900070329     C                   close     fivabwwr
170000070329     C                   endif
170100070329     C                   if        %open(fivatwwr)
170200070329     C                   close     fivatwwr
170300070329     C                   endif
170400070326     C*
170500070326     C* Effettuo la chiamata al CLLE preposto
170600070326     C                   call(e)   'TITVVTC'
170700070326     C                   parm                    parccm
170800070326     C                   parm                    parmbr
170900070326     C                   parm      '2'           paropz
171000070326     C*
171100070326     C                   eval      wrkesito = '2'
171200070404     C*
171300070404     C                   seton                                        LR
171400070326     C*
171500070326     C                   ENDSR     '*CANCL'
171600070326     C***
171700100318
171800100318
171900100318
172000100318     C     normCSV       BEGSR
172100100318     C*
172200100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
172300100318     C* separatori campi
172400100318     C                   if        CharTXT <> *blanks
172500100318     C                   z-add     *zeros        wLenVINDTA        4 0
172600100318     C                   z-add     *zeros        wFlgVINDTA        1 0
172700100318     C                   setoff                                       80
172800100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
172900100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
173000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
173100100318     C                   if        not *in80
173200100318     C                   seton                                        80
173300100318     C                   else
173400100318     C                   setoff                                       80
173500100318     C                   endif
173600100318     C                   endif
173700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
173800100318     C                             and *in80 = *on
173900100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
174000100318     C                   endif
174100100318     C                   enddo
174200100318     C                   endif
174300100318     C*
174400100318     C                   ENDSR
174500100318     C***
174600070326
174700100326
174800100326
174900100326     C*----------------------------------------------------*
175000100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
175100100326     C*----------------------------------------------------*
175200100326     C     REPNSP        BEGSR
175300100326     C*
175400100326     C* NSP => Stacco un numeratore da AZNUM
175500100326     C                   clear                   TRUL33DS
175600100326     C                   eval      I33OPE = *zeros
175700100326     C                   eval      I33CNU = 302
175800100326     C                   eval      I33NUM = 1
175900100326     C                   movel     TRUL33DS      KPJBU
176000100326     C                   call      'TRUL33R'
176100100326     C                   parm                    KPJBA
176200100326     C                   movel     KPJBU         TRUL33DS
176300100326     C                   if        O33ERR = *zeros
176400100326     C                   z-add     O33NRF        VABNSP
176500100326     C                   z-add     O33NRF        VATNSP
176600100326     C                   else
176700100326     C                   SETON                                        31
176800100326     C                   EVAL      vinmsg = %trimr(vinmsg)
176900100326     C                             + ' ' + 'VABNSP VATNSP'
177000100326     C                   endif
177100100326     C*
177200100326     C                   ENDSR
177300010330
177400100326
177500010330
177600000613     C     *inzsr        BEGSR
177700990910     C*
177800990910     C     *entry        plist
177900990920     C                   parm                    tivlrds
178000990921     C                   parm      wrkesito      esito
178100000724     C                   parm                    prmlit
178200000710     C                   parm                    prmfir
178300010330     C*
178400010330     C* CALCOLA LA DATA CORRENTE
178500100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
178600100318     C                   eval      datcor = %dec(%date() : *ISO)
178700000613     C*
178800000613     C                   ENDSR
178900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179000120605X;W_VABRMN;X;X;X;VABLOD;VABCAD;X;VABIND;VABRSD;VABRMA;X;X;X;X;X;VABVLB;X;X;VABPKB
179100120418
179200120418
179300111116
179400100318
