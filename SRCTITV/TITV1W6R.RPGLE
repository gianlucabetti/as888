000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130204     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400100705     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000700040119     FFIVATwwr  O    E             DISK    usropn
000800000313     D*
000900040113     D*------------
001000040113     D* SCHIEDE A PROGRAMMA
001100040113     D*------------
001200080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001300010330     D*----------------------------------------------------
001400010330     D* DICHIARAZIOINE VARIABILI DI WRK
001500010330     D*----------------------------------------------------
001600010330     D dscmz         e ds                  inz
001700010330     D psds           sds
001800010330     D  procname         *PROC
001900010330     D tivlrds       e ds                  extname(tivlr00f)
002000040128     D tisi95ds      e ds
002100100705     D ds15          e ds
002200100413     D fivabds       e ds                  extname(fivab00f)
002300100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002400100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
002500010330     D esito           s              1
002600010330     D prmlit          s             10
002700010330     D prmfir          s             10
002800010330     D wrkesito        s                   like(esito)
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000040510     D parccm          s              8    INZ(*blanks)
003100040119     D parmbr          s             10    INZ(*blanks)
003200040119     D paropz          s              1    INZ(*blanks)
003300040119     D chkcall         s              1    INZ(*blanks)
003400080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003500100705
003600100705     D*------------
003700100705     D* GESTIONE NAZIONI ESTERE
003800100705     D*------------
003900100705     D wNAZ            S              3    INZ
004000100705     D jNAZ            S              5  0 INZ
004100100705     D skNAZISO        S              3    DIM(1000)
004200100705     D skNAZBAR        S              3    DIM(1000)
004300100705
004400100705
004500100326     D*------------------
004600100326     D* DS REPERIMENTO NUMERATORE
004700100326     D*------------------
004800100326     D trul33ds      e ds                  inz
004900100326     D*------------------
005000100326     D* DS ARCHITETTURA
005100100326     D*------------------
005200100326     D kpjba         e ds                  inz
005300100326
005400030715     D*------------------
005500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005600030715     D*------------------
005700030715     D SkSplitFLD      S             10    DIM(100)
005800030715     D SkSplitCSV      S            256    DIM(100)
005900030715     D CharCSV         S              1
006000030715     D CharTXT         S              1
006100030715     D CharNUM         S              1
006200100318     D CharSOS         S              1
006300030715     D posDa           S              3  0 INZ(*zeros)
006400030715     D posA            S              3  0 INZ(*zeros)
006500030715     D i               s              3  0 INZ(1)
006600030715     D wGiro           s              1  0 INZ(*zeros)
006700070823     D wVATNOT_A       s                   LIKE(VATNOT)
006800060331     D wVATNOT_B       s                   LIKE(VATNOT)
006900060331     D wVATNOT_E       s                   LIKE(VATNOT)
007000130204     D wVATNOT_I       s                   LIKE(VATNOT)
007100130204     D wVATNOT_J       s                   LIKE(VATNOT)
007200130204     D wVATNOT_IJ      s             70
007300030822     D*------------------
007400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007500030822     D*------------------
007600030822     D posDaDft        S              3  0 INZ(*zeros)
007700030822     D posADft         S              3  0 INZ(*zeros)
007800030822     D j               s              3  0 INZ(1)
007900030822     D wGiroDft        s              1  0 INZ(*zeros)
008000080418     D*------------------
008100080418     D* Costanti
008200080418     D*------------------
008300080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008400080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008500100318
008600100318     D*------------------
008700100318     D* LINKING A DEFINIZIONI ESTERNE
008800100318     D*------------------
008900100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009000100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009100100318
009200010330
009300010330
009400100705     C*
009500100705     C* Carico le tabelle
009600100705     C                   exsr      cartab
009700100705     C*
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
014700010601     C***
014800000613
014900010601
015000010601
015100010330     C*--------------------------------------------------------
015200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015300010330     C*--------------------------------------------------------
015400010605     C     RWVAB         BEGSR
015500010330     C*
015600010330     C                   if        not %open(tivin00r)
015700010330     C                   open      tivin00r
015800010330     C                   endif
015900021025     C                   if        not %open(fivabwwr)
016000021025     C                   open      fivabwwr
016100010330     C                   endif
016200040119     C*
016300040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016400040119     C                   exsr      prevat
016500040119     C*
016600040119     C                   if        chkcall = '0'
016700040119     C*
016800040119     C                   if        not %open(fivatwwr)
016900040119     C                   open      fivatwwr
017000040119     C                   endif
017100010330     C*
017200040119     C                   clear                   §CTROKVB          7 0
017300040119     C                   clear                   §CTROKVT          7 0
017400040119     C                   clear                   §CTRMO            7 0
017500040119     C                   clear                   §CTRNO            7 0
017600070823     C*
017700070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017800070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017900100318     C***                exsr      inzvar
018000100318     C***                exsr      impvab
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
021700070925     C                   eval      vindta  = ' ' + %trim(vindta)
021800070925     C*
021900080527     C                   exsr      inzvar
022000100318     C                   exsr      normCSV
022100010605     C                   exsr      impvab
022200010601     C*
022300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022400010605     C                   eval      depfil = VABLNP
022500010601     C                   exsr      repfil
022600010601     C                   if        depfil = invfil
022700021025     C                   if        vlrpoi = 999
022800030822     C                   move(P)   invfil        VABFGS
022900021025     C                   else
023000030822     C                   move(P)   vlrpoi        VABFGS
023100021025     C                   endif
023200010601     C*
023300010601     C                   exsr      PREELA
023400010601     C*
023500010604     C* Ebbene...
023600010604     C*
023700040119     C  N31              add       1             §CTROKVB
023800040119     C   32              add       1             §CTRMO
023900040119     C   31              add       1             §CTRNO
024000080418     C*
024100030822     C                   if        wGiro = 2
024200080513     C* Scrivo sempre il VAT
024300080513     C  N31              exsr      wrivat
024400080527     C                   exsr      chkvab
024500080513     C                   endif
024600010604     C*
024700010604     C                   if        *in31 = *off and
024800010604     C                             *in32 = *off
024900010604     C                   eval      vinflg = '1'
025000010604     C                   else
025100010604     C                   eval      vinflg = '2'
025200010604     C                   endif
025300010604     C                   endif
025400010601     C*
025500010604     C                   endif
025600010604     C*
025700010330     C                   else
025800010330     C                   eval      vinflg = '1'
025900010330     C                   endif
026000010601     C*
026100010601     C  N70              update    tivin000
026200010330     C*
026300030822     C  N70              enddo
026400040119     C*
026500080527     C* Verifico dati rimasti "in cannna"...
026600080527     C                   seton                                        65
026700080527     C                   exsr      chkvab
026800080527     C*
026900040119     C                   endif                                                  (endif - chkcall)
027000010601     C*
027100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027200010601     C                   if        cntNonEl = *zeros or
027300010601     C                             flgMulti = '0'
027400010330     C* Se non ci sono record con errori ...
027500010601     C                   if        §ctrno = 0 and
027600010604     C                             §ctrmo = 0 and
027700010601     C                             flgStato <> '2'
027800010330     C* ... restituisco esito OK.
027900010330     C                   eval      wrkesito = '0'
028000010330     C                   else
028100040119     C                   if        §ctrokvb > 0
028200010330     C                   eval      wrkesito = '1'
028300010330     C                   else
028400010615     C                   if        flgOk = '0'
028500010615     C                   eval      wrkesito = '2'
028600010615     C                   else
028700010615     C                   eval      wrkesito = '6'
028800010615     C                   endif
028900010330     C                   endif
029000010330     C                   endif
029100010601     C                   else
029200010601     C                   eval      wrkesito = '9'
029300010601     C                   endif
029400010330     C*
029500010330     C                   if        %open(tivin00r)
029600010330     C                   close     tivin00r
029700010330     C                   endif
029800021025     C                   if        %open(fivabwwr)
029900021025     C                   close     fivabwwr
030000010330     C                   endif
030100040512     C                   if        %open(fivatwwr)
030200040512     C                   close     fivatwwr
030300040512     C                   endif
030400010601     C*
030500010601     C                   if        vlrpoi <> 999
030600010601     C                   eval      invfil = vlrpoi
030700010601     C                   endif
030800010330     C*
030900040119     C                   if        §ctrokvb > 0
031000010601     C                             and invfil > *zeros
031100010330     C                   exsr      invio
031200010330     C                   endif
031300010601     C*
031400010618     C                   if        flgGiro = '1'
031500010601     C                   exsr      endela
031600010618     C                   endif
031700010330     C*
031800010330     C                   ENDSR
031900010330     C***
032000080527
032100080527
032200080527
032300080527     C*----------------------------------------------------*
032400080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032500080527     C*----------------------------------------------------*
032600100413     C     CHKVAB        BEGSR
032700080527     C*
032800080527     C* Scrivo il VAB solo a rottura d spedizione
032900080527     C                   if        depspe = *zeros
033000080527     C  N31              eval      fivabds_sav = fivabds
033100080527     C                   eval      depspe = vabNSP
033200080527     C                   else
033300080527     C                   if        depspe = vabNSP AND *in65 = *off
033400100413     C   51              add       VABNCL        sav_VABNCL
033500100413     C   51              add       VABPKB        sav_VABPKB
033600100413     C   51              add       VABVLB        sav_VABVLB
033700100413     C  N51              z-add     VABNCL        sav_VABNCL
033800100413     C  N51              z-add     VABPKB        sav_VABPKB
033900100413     C  N51              z-add     VABVLB        sav_VABVLB
034000080527     C                   else
034100080527     C  N31              eval      fivabds_ok = fivabds_sav
034200080527     C  N31              exsr      wrivab
034300080527     C  N31              eval      fivabds_sav = fivabds
034400080527     C                   eval      depspe = vabNSP
034500080527     C                   endif
034600080527     C                   endif
034700080527     C*
034800080527     C                   ENDSR
034900040119
035000040119
035100040119
035200040119     C*----------------------------------------------------*
035300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035400040119     C*----------------------------------------------------*
035500040510     C     WRIVAB        BEGSR
035600080513     C*
035700080513     C* Gestisco l'eventuale rottura x numero spedizione
035800080527     C                   if        VABNSP <> depspe or *in65 = *on
035900070301     C                   if        wVATNOT_E <> *blanks
036000070301     C                   eval      VABCTM = '7Q'
036100070301     C                   endif
036200100413     C                   write     fivab000
036300080513     C                   endif
036400040119     C*
036500040119     C                   ENDSR
036600040119
036700040119
036800040119
036900040119     C*----------------------------------------------------*
037000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037100040119     C*----------------------------------------------------*
037200040510     C     WRIVAT        BEGSR
037300060512     C*
037400060512     C* Inizializzo i campi chiave
037500060512     C                   eval      VATFGS = VABFGS
037600060512     C                   eval      VATAAS = VABAAS
037700060512     C                   eval      VATCCM = VABCCM
037800070823     C                   eval      VATNRS = VABNRS
037900060512     C                   eval      VATNSP = VABNSP
038000060512     C                   eval      VATLNP = VABLNP
038100040714     C*
038200040714     C* Solo se VATNOT valorizzato scarico il buffer
038300040714     C                   if        VATNOT <> *blanks
038400040119     C*
038500040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038600040510     C                   if        VATTRC = *blanks
038700040510     C                   eval      VATTRC = 'E'
038800040510     C                   endif
038900040510     C                   write     FIVAT000
039000040119     C                   add       1             §CTROKVT
039100040714     C*
039200040714     C                   endif
039300060331     C*
039400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039500060331     C                   if        wVATNOT_A <> *blanks
039600060331     C                   eval      VATNOT = wVATNOT_A
039700060331     C                   eval      VATTRC = 'A'
039800060331     C                   write     FIVAT000
039900060331     C                   add       1             §CTROKVT
040000060331     C                   endif
040100060331     C*
040200060331     C                   if        wVATNOT_B <> *blanks
040300060331     C                   eval      VATNOT = wVATNOT_B
040400060331     C                   eval      VATTRC = 'B'
040500060331     C                   write     FIVAT000
040600060331     C                   add       1             §CTROKVT
040700060331     C                   endif
040800060331     C*
040900060331     C                   if        wVATNOT_E <> *blanks
041000060331     C                   eval      VATNOT = wVATNOT_E
041100060331     C                   eval      VATTRC = 'E'
041200060331     C                   write     FIVAT000
041300060331     C                   add       1             §CTROKVT
041400060331     C                   endif
041500130204     C*
041600130204     C                   if        wVATNOT_I <> *blanks
041700130204     C                   eval      VATNOT = wVATNOT_I
041800130204     C                   eval      VATTRC = 'I'
041900130204     C                   write     FIVAT000
042000130204     C                   add       1             §CTROKVT
042100130204     C                   endif
042200130204     C*
042300130204     C                   if        wVATNOT_J <> *blanks
042400130204     C                   eval      VATNOT = wVATNOT_J
042500130204     C                   eval      VATTRC = 'J'
042600130204     C                   write     FIVAT000
042700130204     C                   add       1             §CTROKVT
042800130204     C                   endif
042900040119     C*
043000040119     C                   ENDSR
043100040119
043200040119
043300040119
043400040119     C*----------------------------------------------------*
043500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043600040119     C*----------------------------------------------------*
043700040119     C     PREVAT        BEGSR
043800040119     C*
043900040119     C* Compongo il nome del membro da dare al FIVATWWR
044000040119     C                   eval      parmbr = vlrhdl
044100040119     C                   movel     'M'           parmbr
044200040510     C                   eval      parccm = vlrksc
044300040119     C                   eval      paropz = '1'
044400040119     C* Effettuo la chiamata al CLLE preposto
044500040119     C                   call(e)   'TITVVTC'
044600040119     C                   parm                    parccm
044700040119     C                   parm                    parmbr
044800040119     C                   parm                    paropz
044900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045000040119     C                   if        %error
045100040119     C                   movel     '1'           chkcall
045200040119     C                   else
045300040119     C                   movel     '0'           chkcall
045400040119     C                   endif
045500040119     C*
045600040119     C                   ENDSR
045700010601
045800010601
045900010601
046000010330     C*----------------------------------------------------*
046100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046200010330     C*----------------------------------------------------*
046300040119     C     INZVAR        BEGSR
046400080527     C*
046500080527     C                   setoff                                       65
046600010330     C*
046700030822     C* Inizializzo variabili di wrk
046800020204     C                   Z-ADD     *zeros        Num5_0            5 0
046900100705     C                   MOVEL     *blanks       wFlgCAS           1
047000060331     C                   MOVEL     *blanks       wVATNOT_A
047100060331     C                   MOVEL     *blanks       wVATNOT_B
047200060331     C                   MOVEL     *blanks       wVATNOT_E
047300130204     C                   MOVEL     *blanks       wVATNOT_I
047400130204     C                   MOVEL     *blanks       wVATNOT_J
047500130204     C                   MOVEL     *blanks       wVATNOT_IJ
047600030822     C*
047700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047800030822     C                   CLEAR                   SkSplitCSV
047900030822     C                   EVAL      i = 1
048000030822     C                   EVAL      posDa = *zeros
048100030822     C                   EVAL      posA  = *zeros
048200080527     C*
048300080527     C                   CLEAR                   FIVABDS
048400080527     C                   CLEAR                   FIVAT000
048500030822     C*
048600030822     C* Reimposto i valori di default
048700030822     C                   EXSR      DEFCAM
048800030822     C*
048900010330     C                   ENDSR
049000010330     C*----------------------------------------------------*
049100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049200010330     C*----------------------------------------------------*
049300010330     C     DEFCAM        BEGSR
049400030822     C*
049500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049600030715     C* e delimitatore testo.
049700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050000100318     C*
050100100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050200100318     C                   EVAL      CharSOS = CharNUM
050300030822     C*
050400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050500100413     C                   SETOFF                                       5051
050600100413     C                   SELECT
050700100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
050800030715     C                   SETON                                        50
050900100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051000100413     C                   SETON                                        51
051100100413     C                   ENDSL
051200030822     C*
051300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051400030822     C                   EVAL      posDaDft = 1
051500030822     C                   EVAL      posADft  = 0
051600030822     C                   EVAL      wGiroDft = 0
051700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051800030822     C                             posDaDft > 0
051900030822     C*
052000030822     C* Gestisco il 1° giro
052100030822     C                   IF        wGiroDft = 0
052200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052400030822     C* Incremento il contatore dei "giri"
052500030822     C                   EVAL      wGiroDft = 1
052600030822     C                   ELSE
052700030822     C                   EVAL      posDaDft = posADft
052800030822     C                   ENDIF
052900030822     C* Eseguo lo scan x trovare la fine del campo corrente
053000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053100030822     C*
053200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053400030822     C                   IF        posDaDft > 0 AND
053500030822     C                             posADft  > 0
053600030822     C* NCL
053700030822     C                   IF        %subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):1:3)
054000030822     C                             = 'NCL'
054100030822     C                   EVAL      PiStr=%trim(%subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054400030822     C                   EXSR      CHKNUM
054500030822     C                   IF        PiInt=*on
054600030822     C                   Z-ADD     PiVal         VABNCL
054700030822     C                   ENDIF
054800030822     C                   ENDIF
054900030822     C* CCM
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'CCM'
055400030822     C                   EVAL      PiStr=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   EXSR      CHKNUM
055800030822     C                   IF        PiInt=*on
055900030822     C                   Z-ADD     PiVal         VABCCM
056000030822     C                   ENDIF
056100030822     C                   ENDIF
056200030822     C* LNP
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'LNP'
056700030822     C                   EVAL      PiStr=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   EXSR      CHKNUM
057100030822     C                   IF        PiInt=*on
057200030822     C                   Z-ADD     PiVal         VABLNP
057300030822     C                   ENDIF
057400030822     C                   ENDIF
057500030822     C* NRS
057600030822     C                   IF        %subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):1:3)
057900030822     C                             = 'NRS'
058000030822     C                   EVAL      PiStr=%trim(%subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058300030822     C                   EXSR      CHKNUM
058400030822     C                   IF        PiInt=*on
058500030822     C                   Z-ADD     PiVal         VABNRS
058600030822     C                   ENDIF
058700030822     C                   ENDIF
058800030822     C* CTR
058900030822     C                   IF        %subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):1:3)
059200030822     C                             = 'CTR'
059300030822     C                   EVAL      PiStr=%trim(%subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600030822     C                   EXSR      CHKNUM
059700030822     C                   IF        PiInt=*on
059800030822     C                   Z-ADD     PiVal         VABCTR
059900030822     C                   ENDIF
060000030822     C                   ENDIF
060100030822     C* PKB
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'PKB'
060600030822     C                   EVAL      PiStr=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   EXSR      CHKNUM
061000030822     C                   IF        PiNum=*on
061100030822     C                   Z-ADD     PiVal         VABPKB
061200030822     C                   ENDIF
061300030822     C                   ENDIF
061400030822     C* VLB
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'VLB'
061900030822     C                   EVAL      PiStr=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   EXSR      CHKNUM
062300030822     C                   IF        PiNum=*on
062400030822     C                   Z-ADD     PiVal         VABVLB
062500030822     C                   ENDIF
062600030822     C                   ENDIF
062700030822     C* QFT
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'QFT'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiNum=*on
063700030822     C                   Z-ADD     PiVal         VABQFT
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* CBO
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'CBO'
064500030822     C                   EVAL      VABCBO=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   ENDIF
064900030822     C* TSP
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'TSP'
065400030822     C                   EVAL      VABTSP=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   ENDIF
065800030822     C* VAS
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'VAS'
066300030822     C                   EVAL      VABVAS=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* VCA
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'VCA'
067200030822     C                   EVAL      VABVCA=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600070925     C* TIC
067700070925     C                   IF        %subst(
067800070925     C                             %subst(vlrppt:posDaDft+1:
067900070925     C                             posADft-posDaDft-1):1:3)
068000070925     C                             = 'TIC'
068100070925     C                   EVAL      VABTIC=%trim(%subst(
068200070925     C                             %subst(vlrppt:posDaDft+1:
068300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400070925     C                   ENDIF
068500030822     C* GCA
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'GCA'
069000030822     C                   EVAL      VABGCA=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400030822     C* CTM
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'CTM'
069900030822     C                   EVAL      VABCTM=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* FFD
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'FFD'
070800030822     C                   EVAL      VABFFD=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* VAD
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'VAD'
071700030822     C                   EVAL      VABVAD=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* GMA
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'GMA'
072600030822     C                   EVAL      VABGMA=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* GGA
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'GGA'
073500030822     C                   EVAL      VABGGA=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* GVA
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'GVA'
074400030822     C                   EVAL      VABGVA=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* TC1
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'TC1'
075300030822     C                   EVAL      VABTC1=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* TC2
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'TC2'
076200030822     C                   EVAL      VABTC2=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600040714     C* VATTRC
076700040714     C                   IF        %subst(
076800040714     C                             %subst(vlrppt:posDaDft+1:
076900040714     C                             posADft-posDaDft-1):1:3)
077000040714     C                             = 'TRC'
077100040714     C                   EVAL      VATTRC=%trim(%subst(
077200040714     C                             %subst(vlrppt:posDaDft+1:
077300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400040714     C                   ENDIF
077500030822     C* ...
077600030822     C                   ENDIF
077700030822     C                   ENDDO
077800020204     C*
077900010330     C                   ENDSR
078000010607     C*----------------------------------------------------*
078100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078200010607     C*----------------------------------------------------*
078300010607     C     IMPVAB        BEGSR
078400010607     C*
078500010607     C                   SETOFF                                       3132
078600030515     C*
078700010607     C                   MOVEL     datcor        VABAAS
078800010607     C                   MOVE      datcor        VABMGS
078900010607     C*
079000030715     C**********
079100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079200030715     C**********
079300030715     C                   IF        wGiro = *zeros
079400040113     C*
079500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
079600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
079700080429     C                             %trim(intcol(3))+%trim(intcol(4))+
079800080429     C                             %trim(intcol(5))<>*blanks
079900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080000080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
080100080429     C                                      %trim(intcol(5))
080200070823     C     ';':CharCSV   XLATE     vindta        vindta
080300040510     C                   ENDIF
080400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080500031201     C     minu:maiu     XLATE     vindta        vindta
080600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080700030715     C                   DOW       posDa <= %len(%trim(vindta))
080800030715     C*
080900030715     C* Gestisco il 1° campo
081000030715     C                   IF        i = 1
081100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081200030715     C                   EVAL      posDa = 1
081300030715     C* Eseguo lo scan x trovare la fine del primo campo
081400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081500030715     C* A questo punto "estraggo" il campo corrente
081600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081700030715     C                                                    (posA-posDa))
081800030715     C* X i campi successivi al 1°
081900030715     C                   ELSE
082000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082100030715     C                   EVAL      posDa = posA + 1
082200030715     C* Eseguo lo scan x trovare la fine del campo corrente
082300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082400030715     C* Gestisco l'ultimo campo
082500030715     C                   IF        posA = *zeros
082600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082700030715     C* A questo punto "estraggo" il campo corrente
082800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082900030715     C                                                    (posA-posDa))
083000030715     C                   LEAVE
083100030715     C                   ELSE
083200030715     C* A questo punto "estraggo" il campo corrente
083300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083400030715     C                                                    (posA-posDa))
083500030715     C                   ENDIF
083600030715     C                   ENDIF
083700030715     C* Incremento il contatore d campo
083800030715     C                   EVAL      i = i +1
083900030715     C                   ENDDO
084000030715     C*
084100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084200030715     C* delimitazione testo
084300030715     C                   EVAL      i = 1
084400030715     C                   DOW       i < %elem(SkSplitFLD)
084500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084700030715     C                   EVAL      i = i + 1
084800030715     C                   ENDDO
084900030715     C                   EVAL      wGiro = 1
085000030715     C                   ELSE
085100030715     C                   EVAL      wGiro = 2
085200031204     C**********
085300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085400031204     C* d separatore campo
085500031204     C**********
085600031204     C                   EVAL      vindta = %trim(vindta)
085700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085800031204     C                   EVAL      vindta = %subst(vindta:2)
085900031204     C                   ENDDO
086000031204     C*
086100031204     C                   Z-ADD     *zeros        lunghInput        4 0
086200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086600031204     C                   ENDDO
086700030715     C**********
086800030715     C* Effettuo lo split del campo dati d input
086900030715     C**********
087000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087100030715     C                   DOW       posDa <= %len(%trim(vindta))
087200030715     C*
087300030715     C* Gestisco il 1° campo
087400030715     C                   IF        i = 1
087500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087600030715     C                   EVAL      posDa = 1
087700030715     C* Eseguo lo scan x trovare la fine del primo campo
087800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087900030715     C* A questo punto "estraggo" il campo corrente
088000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088100030715     C                                                    (posA-posDa))
088200030715     C* X i campi successivi al 1°
088300030715     C                   ELSE
088400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088500030715     C                   EVAL      posDa = posA + 1
088600031204     C* Verifico che nn vi sia il campo nn valorizzato
088700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088800031204     C* Se campo nn valorizzato skippo al prossimo
088900031204     C                   EVAL      SkSplitCSV(i) = *blanks
089000031204     C                   EVAL      posA  = posA + 1
089100031204     C                   ELSE
089200030715     C* Eseguo lo scan x trovare la fine del campo corrente
089300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089400030715     C* Gestisco l'ultimo campo
089500030715     C                   IF        posA = *zeros
089600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089700030715     C* A questo punto "estraggo" il campo corrente
089800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089900030715     C                                                    (posA-posDa))
090000030715     C                   LEAVE
090100030715     C                   ELSE
090200030715     C* A questo punto "estraggo" il campo corrente
090300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090400030715     C                                                    (posA-posDa))
090500030715     C                   ENDIF
090600031204     C                   ENDIF
090700030715     C                   ENDIF
090800030715     C* Incremento il contatore d campo
090900030715     C                   EVAL      i = i +1
091000030715     C                   ENDDO
091100030715     C*
091200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091300030715     C* delimitazione testo
091400030715     C                   EVAL      i = 1
091500030715     C                   DOW       i < %elem(SkSplitCSV)
091600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091800030715     C                   EVAL      i = i + 1
091900030715     C                   ENDDO
092000100326     C*
092100100326     C* Stacco un progressivo x numero spedizione
092200100705     C***                EXSR      repNSP
092300100326     C*
092400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092500030715     C                   EVAL      i = 1
092600030715     C                   DOW       i < %elem(SkSplitFLD)
092700030715     C*
092800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093000030715     C                             SkSplitCSV(i) <> *blanks
093100030721     C*** ATB
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
093300030721     C                   EVAL      VABATB = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030715     C*** RSD
093600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
093700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
093800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
093900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094000030715     C* ==
094100030715     C                   ENDIF
094200030721     C*** RD2
094300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
094400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
094500030721     C                   ENDIF
094600030715     C*** IND
094700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
094800030715     C                   EVAL      VABIND = SkSplitCSV(i)
094900030715     C                   ENDIF
095000030715     C*** LOD
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
095200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030715     C*** PRD
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
095600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
095700030715     C                   ENDIF
095800030721     C*** NZD
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096100030721     C                   ENDIF
096200030715     C*** RMA
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
096400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** NOT
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
096800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030715     C*** NT2
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
097200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030715     C*** VCA
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
097600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** VAS
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** FFD
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
098400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030721     C*** GC1
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
098800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** GC2
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
099200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** TSP
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
099600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
099700030721     C                   ENDIF
099800040510     C*** CBO
099900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
100100040510     C                   ENDIF
100200030721     C*** NAS
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
100400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** TIC
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
100800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** GCA
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
101200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** XCO
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
101600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** CTM
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** TCR
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
102400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** CTS
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
102800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** FTM
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
103200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** VAD
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
103600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** GMA
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** GGA
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
104400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** GVA
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
104800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** TC1
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
105200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** TC2
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
105600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** SCL
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** RMO
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
106400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** NMO
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
106800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*
107100030721     C* Reperisco quindi i campi numerici...
107200040510     C*** NSP
107300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
107400040510     C                   EVAL      PiStr=SkSplitCSV(i)
107500040510     C                   EXSR      CHKNUM
107600040510     C                   IF        PiInt=*on
107700040510     C                   Z-ADD     PiVal         VABNSP
107800040510     C                   ELSE
107900040510     C                   SETON                                        31
108000040510     C                   EVAL      VABNSP = *zeros
108100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
108200040510     C                             + ' ' + 'VABNSP'
108300040510     C                   ENDIF
108400040510     C                   ENDIF
108500071106     C*** RMN
108600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
108700071106     C                   EVAL      PiStr=SkSplitCSV(i)
108800071106     C                   EXSR      CHKNUM
108900071106     C                   IF        PiInt=*on
109000071106     C                   Z-ADD     PiVal         VABRMN
109100071106     C                   ELSE
109200071106     C                   SETON                                        32
109300071106     C                   EVAL      VABRMN = *zeros
109400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
109500071106     C                             + ' ' + 'VABRMN'
109600071106     C                   ENDIF
109700071106     C                   ENDIF
109800030721     C*** CAD
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiInt=*on
110300030721     C                   Z-ADD     PiVal         Num5_0
110400030721     C                   MOVEL(p)  Num5_0        VABCAD
110500030721     C                   ELSE
110600030721     C                   SETON                                        32
110700030721     C                   EVAL      VABCAD = *zeros
110800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110900030721     C                             + ' ' + 'VABCAD'
111000030721     C                   ENDIF
111100030721     C                   ENDIF
111200030721     C*** NCL
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiInt=*on
111700030721     C                   Z-ADD     PiVal         VABNCL
111800030721     C                   ELSE
111900030721     C                   SETON                                        32
112000030721     C                   EVAL      VABNCL = *zeros
112100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112200040122     C                             + ' ' + 'VABNCL'
112300030721     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** PKB
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112700030721     C                   EVAL      PiStr=SkSplitCSV(i)
112800030721     C                   EXSR      CHKNUM
112900030721     C                   IF        PiNum=*on
113000030721     C                   Z-ADD     PiVal         VABPKB
113100030721     C                   ELSE
113200030721     C                   SETON                                        32
113300030721     C                   EVAL      VABPKB = *zeros
113400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113500030721     C                             + ' ' + 'VABPKB'
113600030721     C                   ENDIF
113700030721     C                   ENDIF
113800030721     C*** CAS
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114000030721     C                   EVAL      PiStr=SkSplitCSV(i)
114100100705     C                   IF        PiStr<>*blanks
114200100705     C                   EVAL      wFlgCAS = '1'
114300100705     C                   EVAL      VABVCA='EUR'
114400100705     C                   ENDIF
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiNum=*on
114700030721     C                   Z-ADD     PiVal         VABCAS
114800030721     C                   ELSE
114900030721     C                   SETON                                        32
115000030721     C                   EVAL      VABCAS = *zeros
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200030721     C                             + ' ' + 'VABCAS'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** CCM
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on      AND
116000030721     C                             PiVal<=9999999 AND
116100030721     C                             PiVal>*zeros
116200030721     C                   Z-ADD     PiVal         VABCCM
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      VABCCM = *zeros
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700040510     C                             + ' ' + 'VABCCM'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** LNP
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiInt=*on    AND
117500030721     C                             PiVal<=999   AND
117600030721     C                             PiVal>*zeros
117700030721     C                   Z-ADD     PiVal         VABLNP
117800030721     C                   ELSE
117900040715     C                   SETON                                        32
118000030721     C                   EVAL      VABLNP = *zeros
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200040510     C                             + ' ' + 'VABLNP'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500030721     C*** NRS
118600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
118700030721     C                   EVAL      PiStr=SkSplitCSV(i)
118800030721     C                   EXSR      CHKNUM
118900030721     C                   IF        PiInt=*on AND
119000030721     C                             PiVal<=99
119100030721     C                   Z-ADD     PiVal         VABNRS
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500030721     C                             + ' ' + 'VABNRS'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** LNA
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiInt=*on    AND
120300030721     C                             PiVal<=999   AND
120400030721     C                             PiVal>*zeros
120500030721     C                   Z-ADD     PiVal         VABLNA
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      VABLNA = *zeros
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABLNA'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** CTR
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on  AND
121800030721     C                             PiVal<=999
121900030721     C                   Z-ADD     PiVal         VABCTR
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABCTR'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** IAS
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiNum=*on
123100030721     C                   Z-ADD     PiVal         VABIAS
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABIAS'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** VLB
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiNum=*on
124300030721     C                   Z-ADD     PiVal         VABVLB
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABVLB'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** QFT
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiNum=*on
125500030721     C                   Z-ADD     PiVal         VABQFT
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900030721     C                             + ' ' + 'VABQFT'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** NCD
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiInt=*on      AND
126700030721     C                             PiVal<=9999999
126800030721     C                   Z-ADD     PiVal         VABNCD
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABNCD'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500030721     C*** NCA
127600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
127700030721     C                   EVAL      PiStr=SkSplitCSV(i)
127800030721     C                   EXSR      CHKNUM
127900030721     C                   IF        PiInt=*on      AND
128000030721     C                             PiVal<=9999999
128100030721     C                   Z-ADD     PiVal         VABNCA
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABNCA'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** ZNC
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on AND
129300030721     C                             PiVal<=99
129400030721     C                   Z-ADD     PiVal         VABZNC
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABZNC'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** DCR
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on       AND
130600030721     C                             PiVal<=99999999
130700030721     C                   Z-ADD     PiVal         VABDCR
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABDCR'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** HCR
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on   AND
131900030721     C                             PiVal<=9999
132000030721     C                   Z-ADD     PiVal         VABHCR
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABHCR'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** VMD
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiNum=*on
133200030721     C                   Z-ADD     PiVal         VABVMD
133300030721     C                   ELSE
133400030721     C                   SETON                                        32
133500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133600030721     C                             + ' ' + 'VABVMD'
133700030721     C                   ENDIF
133800030721     C                   ENDIF
133900030721     C*** ANT
134000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
134100030721     C                   EVAL      PiStr=SkSplitCSV(i)
134200030721     C                   EXSR      CHKNUM
134300030721     C                   IF        PiInt=*on        AND
134400030721     C                             PiVal<=999999999
134500030721     C                   Z-ADD     PiVal         VABANT
134600030721     C                   ELSE
134700030721     C                   SETON                                        32
134800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134900030721     C                             + ' ' + 'VABANT'
135000030721     C                   ENDIF
135100030721     C                   ENDIF
135200030721     C*** CMO
135300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135400030721     C                   EVAL      PiStr=SkSplitCSV(i)
135500030721     C                   EXSR      CHKNUM
135600030721     C                   IF        PiInt=*on
135700030721     C                   Z-ADD     PiVal         Num5_0
135800030721     C                   MOVEL(p)  Num5_0        VABCMO
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      VABCMO = *zeros
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABCMO'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600040510     C*
136700040510     C* Reperisco i campi del FIVAT
136800040510     C*** VATNOT
136900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
137100040510     C                   ENDIF
137200040510     C*** VATTRC
137300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
137500040510     C                   ENDIF
137600060331     C*** VATNOT_A
137700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
137800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
137900060331     C                   ENDIF
138000060331     C*** VATNOT_B
138100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
138200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138300060331     C                   ENDIF
138400060331     C*** VATNOT_E
138500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
138600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
138700060331     C                   ENDIF
138800070823     C*
138900071029     C***  ===>  Gestione campi "particolari" dentro ciclo
139000100413     C*** NZD/CTM/CTR
139100100413     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
139200100413     C                   EVAL      VABNZD=SkSplitCSV(i)
139300100705     C*
139400100705     C* ...passo dal codice ISO a quello "Bartolini"
139500100705     C                   Z-ADD     1             jNAZ
139600100705     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
139700100705     C                   IF        %found
139800100705     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
139900100705     C                   ENDIF
140000100705     C*
140100100413     C                   IF        VABNZD<> *blanks
140200100413     C                   EVAL      VABCTM='7R'
140300100413     C                   EVAL      VABCTR=352
140400100413     C                   ELSE
140500100413     C                   EVAL      VABCTM='7Q'
140600120918     C***                EVAL      VABCTR=000
140700120918     C                   EVAL      VABCTR=001
140800100413     C                   ENDIF
140900100326     C                   ENDIF
141000110802     C*** NSP / RMN / RMA
141100100705     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
141200110802     C                   EVAL      VABRMA=SkSplitCSV(i)
141300100705     C                   EVAL      PiStr=SkSplitCSV(i)
141400100705     C                   EXSR      CHKNUM
141500100705     C                   IF        PiInt=*on
141600100705     C                   Z-ADD     PiVal         VABRMN
141700100705     C                   Z-ADD     PiVal         VABNSP
141800100705     C                   ELSE
141900100705     C                   SETON                                        31
142000100705     C                   EVAL      VABRMN = *zeros
142100100705     C                   EVAL      vinmsg = %trimr(vinmsg)
142200110802     C                             + ' ' + 'VABRMN VABNSP VATNSP'
142300100705     C                   ENDIF
142400100705     C                   ENDIF
142500130204     C*
142600130204     C*** VATNOT_I/J
142700130204     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
142800130204     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
142900130204     C                   EVAL      wVATNOT_I=%subst(wVATNOT_IJ:1:35)
143000130204     C                   EVAL      wVATNOT_J=%subst(wVATNOT_IJ:36:35)
143100130204     C                   ENDIF
143200070925     C*
143300071029     C***  <===  -----------------------------------------
143400040510     C*
143500030715     C                   ENDIF
143600030715     C                   EVAL      i = i + 1
143700030715     C                   ENDDO
143800070925     C*
143900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144000070925     C*
144100071029     C***  <===  -----------------------------------------
144200070925     C*
144300070925     C* Considerazioni finali su CBO/CAS
144400100318     C                   IF        wFlgCAS = '1'    OR
144500100318     C                             VABCAS  > *zeros
144600070925     C                   IF        VABCBO = '1'
144700070925     C                   EVAL      VABCBO = '4'
144800070925     C                   ENDIF
144900070925     C                   IF        VABCBO = '2'
145000070925     C                   EVAL      VABCBO = '6'
145100070925     C                   ENDIF
145200070925     C                   ENDIF
145300070925     C***  <===  ----------------------------
145400040510     C*
145500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
145600040510     C                   IF        VABNSP = *zeros
145700040510     C                   SETON                                        31
145800040510     C                   ENDIF
145900040715     C*
146000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146100040715     C                   IF        VABLNP = *zeros
146200040715     C                   SETON                                        31
146300040715     C                   ENDIF
146400040510     C*
146500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
146600040510     C                   IF        VABRMN = *zeros
146700040510     C                   Z-ADD     VABNSP        VABRMN
146800040510     C                   ENDIF
146900040510     C*
147000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
147100040510     C                   IF        VABRMA = *blanks
147200040714     C                   MOVEL     VABRMN        VABRMA
147300040510     C                   ENDIF
147400040128     C*
147500040510     C* Se provincia nn valorizzata la reperisco
147600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
147700040510     C                   IF        VABPRD = *blanks
147800040128     C                   CLEAR                   TISI95DS
147900040128     C                   EVAL      I95TCN = '3'
148000040128     C                   Z-ADD     datcor        I95DAT
148100040128     C                   EVAL      I95NAR = VABNZD
148200040128     C                   EVAL      I95CAP = VABCAD
148300040128     C                   EVAL      I95LOC = VABLOD
148400040128     C                   CALL      'TISI95R'
148500040128     C                   PARM                    TISI95DS
148600040128     C                   EVAL      VABPRD = O95PRV
148700040510     C                   ENDIF
148800020204     C*
148900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
149000020204     C                   EXSR      CHKIMPDIV
149100030715     C                   ENDIF
149200020204     C*
149300010607     C                   ENDSR
149400010607     C*----------------------------------------------------*
149500010601
149600020204
149700020204     C*----------------------------------------------------*
149800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
149900020204     C*----------------------------------------------------*
150000020204     C     CHKIMPDIV     BEGSR
150100020204     C*
150200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
150300020204     C                   Z-ADD     *zeros        wrkDec            9 9
150400020204     C*
150500020204     C* Come prima cosa effettuo considerazioni sulla divisa
150600020204     C                   IF        vabIAS > *zeros
150700020204     C                   IF        vabVAS <> 'EUR'
150800020204     C                   EVAL      vabVAS =  'ITL'
150900020204     C                   ENDIF
151000020204     C                   ENDIF
151100020204     C*
151200020204     C                   IF        vabCAS > *zeros
151300020204     C                   IF        vabVCA <> 'EUR'
151400020204     C                   EVAL      vabVCA =  'ITL'
151500020204     C                   ENDIF
151600020204     C                   ENDIF
151700020204     C*
151800020204     C                   IF        vabVMD > *zeros
151900020204     C                   IF        vabVAD <> 'EUR'
152000020204     C                   EVAL      vabVAD =  'ITL'
152100020204     C                   ENDIF
152200020204     C                   ENDIF
152300020204     C*
152400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152500020204     C                   Z-ADD     vabIAS        wrkDec
152600020204     C                   IF        wrkDec > *zeros
152700020204     C                   IF        vabVAS = 'ITL'
152800020204     C                   EVAL      vabIAS = *zeros
152900020204     C                   ENDIF
153000020204     C                   ENDIF
153100020204     C*
153200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
153300020204     C                   Z-ADD     vabCAS        wrkDec
153400020204     C                   IF        wrkDec > *zeros
153500020204     C                   IF        vabVCA = 'ITL'
153600020204     C                   EVAL      vabCAS = *zeros
153700020204     C                   ENDIF
153800020204     C                   ENDIF
153900020204     C*
154000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
154100020204     C                   Z-ADD     vabVMD        wrkDec
154200020204     C                   IF        wrkDec > *zeros
154300020204     C                   IF        vabVAD = 'ITL'
154400020204     C                   EVAL      vabVMD = *zeros
154500020204     C                   ENDIF
154600020204     C                   ENDIF
154700020204     C*
154800020204     C                   ENDSR
154900020204     C***
155000010330
155100100318
155200100318
155300100318     C*----------------------------------------------------*
155400100318     C*  CONTROLLO NUMERICITA' CAMPI
155500100318     C*----------------------------------------------------*
155600100318     C     CHKNUM        BEGSR
155700100318     C*
155800100318     C                   IF        PiDecChr = *blanks
155900100318     C                   EVAL      PiDecChr = CharNUM
156000100318     C                   ENDIF
156100100318     C*
156200100318     C                   callp     UBISNUM_Check(PiStr
156300100318     C                                          :PiDecChr
156400100318     C                                          :PiVal
156500100318     C                                          :PiNum
156600100318     C                                          :PiInt)
156700100318     C*
156800100318     C                   ENDSR
156900100318     C***
157000010601
157100010601
157200010601
157300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157400010601     C     repfil        BEGSR
157500010601     C*
157600010601     C                   if        invfil = *zeros and
157700010601     C                             depfil > *zeros and
157800010629     C                             (vinflg = *blanks or
157900010629     C                              vinflg = *zeros)
158000010601     C
158100010601     C                   eval      invfil = depfil
158200010601     C                   endif
158300010601     C*
158400010601     C                   if        depfil <> invfil and
158500010601     C                             invfil > *zeros
158600010601     C                   eval      flgMulti = '1'
158700010601     C                   if        vinflg = *blanks
158800010601     C                   add       1             cntNonEl
158900010601     C                   endif
159000010601     C                   endif
159100010601     C*
159200010601     C                   if        vinflg = '2'
159300010601     C                   eval      flgStato = '2'
159400010601     C                   endif
159500010601     C*
159600010601     C                   ENDSR
159700010601     C***
159800010601
159900010601
160000010601
160100010330
160200040119      /TITLE Invio dei dati al punto operativo.
160300040119     C     invio         BEGSR
160400040119     C*
160500040119     C* 1° invio FIVAT
160600040119     C                   reset                   dscmz
160700040119     C                   move      vlrpoi        cmzdst
160800040119     C                   eval      cmzfld = 'FIVATWWR'
160900040119     C                   eval      cmzmbd = vlrhdl
161000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161100040119     C***                if        prmfir = *blanks
161200040119     C                   eval      cmzfla = 'FIVAT00F'
161300040119     C                   eval      cmzmba = 'FIVAT00F'
161400040119     C***                else
161500040119     C***                eval      cmzfla = prmfir
161600040119     C***                eval      cmzmba = prmfir
161700040119     C***                endif
161800040119     C                   eval      cmznrr = *zeros
161900040119     C                   move      §ctrokvt      cmznrr
162000040119     C                   eval      cmzlba = vlrfl1
162100040119     C                   call(e)   'TIS711C'
162200040119     C                   parm                    dscmz
162300040119     C                   parm      *blanks       esito
162400040119     C                   if        %error
162500040119     C                             or cmzerr = '1'
162600040119     C                             or esito  = '1'
162700040119     C                   eval      wrkesito = '3'
162800040119     C                   else
162900040119     C*
163000040119     C* 2° invio FIVAB
163100040119     C                   reset                   dscmz
163200040119     C                   move      vlrpoi        cmzdst
163300040119     C                   eval      cmzfld = vlrfou
163400040119     C                   eval      cmzmbd = vlrhdl
163500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163600040119     C***                if        prmfir = *blanks
163700040119     C                   eval      cmzfla = 'FIVAB00F'
163800040119     C                   eval      cmzmba = 'FIVAB00F'
163900040119     C***                else
164000040119     C***                eval      cmzfla = prmfir
164100040119     C***                eval      cmzmba = prmfir
164200040119     C***                endif
164300040119     C                   eval      cmznrr = *zeros
164400040119     C                   move      §ctrokvb      cmznrr
164500040119     C                   eval      cmzlba = vlrfl1
164600040119     C                   call(e)   'TIS711C'
164700040119     C                   parm                    dscmz
164800040119     C                   parm      *blanks       esito
164900040119     C                   if        %error
165000040119     C                             or cmzerr = '1'
165100040119     C                             or esito  = '1'
165200040119     C                   eval      wrkesito = '3'
165300040119     C                   endif
165400040119     C                   endif
165500040119     C*
165600040119     C                   ENDSR
165700040119     C***
165800010601
165900010601
166000010601
166100010601
166200010601      /TITLE Invio dei dati al punto operativo.
166300010601     C     opeini        BEGSR
166400010601     C*
166500010601     C* Inizializzo flag e contatori operativi
166600010601     C                   movel     '0'           flgGiro           1
166700010601     C                   movel     '0'           flgMulti          1
166800010601     C                   movel     '1'           flgStato          1
166900010615     C                   movel     '0'           flgOk             1
167000010601     C                   z-add     *zeros        cntNonEl         10 0
167100010601     C                   z-add     *zeros        depfil            3 0
167200010601     C                   z-add     *zeros        invfil            3 0
167300010601     C*
167400010601     C                   ENDSR
167500010601     C***
167600070326
167700070326
167800070326
167900070326
168000070326     C     *pssr         BEGSR
168100070329     C*
168200070329     C                   if        %open(tivin00r)
168300070329     C                   close     tivin00r
168400070329     C                   endif
168500070329     C                   if        %open(fivabwwr)
168600070329     C                   close     fivabwwr
168700070329     C                   endif
168800070329     C                   if        %open(fivatwwr)
168900070329     C                   close     fivatwwr
169000070329     C                   endif
169100070326     C*
169200070326     C* Effettuo la chiamata al CLLE preposto
169300070326     C                   call(e)   'TITVVTC'
169400070326     C                   parm                    parccm
169500070326     C                   parm                    parmbr
169600070326     C                   parm      '2'           paropz
169700070326     C*
169800070326     C                   eval      wrkesito = '2'
169900070404     C*
170000070404     C                   seton                                        LR
170100070326     C*
170200070326     C                   ENDSR     '*CANCL'
170300070326     C***
170400100318
170500100318
170600100318
170700100318     C     normCSV       BEGSR
170800100318     C*
170900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
171000100318     C* separatori campi
171100100318     C                   if        CharTXT <> *blanks
171200100318     C                   z-add     *zeros        wLenVINDTA        4 0
171300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
171400100318     C                   setoff                                       80
171500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
171600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
171700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
171800100318     C                   if        not *in80
171900100318     C                   seton                                        80
172000100318     C                   else
172100100318     C                   setoff                                       80
172200100318     C                   endif
172300100318     C                   endif
172400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
172500100318     C                             and *in80 = *on
172600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
172700100318     C                   endif
172800100318     C                   enddo
172900100318     C                   endif
173000100318     C*
173100100318     C                   ENDSR
173200100318     C***
173300070326
173400100326
173500100326
173600100326     C*----------------------------------------------------*
173700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
173800100326     C*----------------------------------------------------*
173900100326     C     REPNSP        BEGSR
174000100326     C*
174100100326     C* NSP => Stacco un numeratore da AZNUM
174200100326     C                   clear                   TRUL33DS
174300100326     C                   eval      I33OPE = *zeros
174400100326     C                   eval      I33CNU = 302
174500100326     C                   eval      I33NUM = 1
174600100326     C                   movel     TRUL33DS      KPJBU
174700100326     C                   call      'TRUL33R'
174800100326     C                   parm                    KPJBA
174900100326     C                   movel     KPJBU         TRUL33DS
175000100326     C                   if        O33ERR = *zeros
175100100326     C                   z-add     O33NRF        VABNSP
175200100326     C                   z-add     O33NRF        VATNSP
175300100326     C                   else
175400100326     C                   SETON                                        31
175500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
175600100326     C                             + ' ' + 'VABNSP VATNSP'
175700100326     C                   endif
175800100326     C*
175900100326     C                   ENDSR
176000010330
176100100705
176200100705
176300100705     C*--------------------------------------------------------
176400100705     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
176500100705     C*--------------------------------------------------------
176600100705     C     CARTAB        BEGSR
176700100705     C*
176800100705     C* TABELLA '15' - NAZIONI
176900100705     C                   clear                   skNAZISO
177000100705     C                   clear                   skNAZBAR
177100100705     C                   eval      tblKUT = 1
177200100705     C                   eval      tblCOD = '15'
177300100705     C     KEYtabP       setll     tabel00f
177400100705     C     KEYtabP       reade     tabel00f
177500100705     C                   dow       not %eof(tabel00f)
177600100705     C                   if        tblFLG = *blanks
177700100705     C                   movel(p)  tblUNI        ds15
177800100705     C                   add       1             jNAZ
177900100705     C                   eval      skNAZBAR(jNAZ) = tblKEY
178000100705     C                   eval      skNAZISO(jNAZ) = §15COD
178100100705     C                   endif
178200100705     C     KEYtabP       reade     tabel00f
178300100705     C                   enddo
178400100705     C*
178500100705     C                   ENDSR
178600100705     C***
178700100705
178800100326
178900010330
179000000613     C     *inzsr        BEGSR
179100990910     C*
179200990910     C     *entry        plist
179300990920     C                   parm                    tivlrds
179400990921     C                   parm      wrkesito      esito
179500000724     C                   parm                    prmlit
179600000710     C                   parm                    prmfir
179700100705     C*
179800100705     C* Chiave su TABEL00F - parziale
179900100705     C     KEYtabP       KLIST
180000100705     C                   KFLD                    tblKUT
180100100705     C                   KFLD                    tblCOD
180200010330     C*
180300010330     C* CALCOLA LA DATA CORRENTE
180400100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
180500100318     C                   eval      datcor = %dec(%date() : *ISO)
180600000613     C*
180700000613     C                   ENDSR
180800000613     C***
180900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181000100705W_VABRMN;VATNOT_E;VABRSD;VABRD2;VABIND;VABNOT;VABCAD;VABLOD;VABPRD;
181100130204W_VABNZD;VATNOT_B;VABNCL;VABPKB;VABCAS;VABCBO;VABFFD;VABNT2;W_VATNOT_I;VABIAS;
181200100318
181300100318
181400100318
