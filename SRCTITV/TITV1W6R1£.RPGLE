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
007300160211     D wVATNOT_S       s                   LIKE(VATNOT)
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
009500100705     C*
009600100705     C* Carico le tabelle
009700100705     C                   exsr      cartab
009800100705     C*
009900000913     C                   reset                   rrnum
010000990921     C                   reset                   esito
010100990921     C                   reset                   wrkesito
010200010601     C*
010300010601     C                   exsr      opeini
010400010605     C                   exsr      rwvab
010500010601     C*
010600010601     C                   seton                                        lr
010700010601
010800010601
010900010601
011000010601     C*--------------------------------------------------------
011100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011200010601     C*--------------------------------------------------------
011300010601     C     PREELA        BEGSR
011400010601     C*
011500010601     C* SE OCCORRE SPEDIRE IN FILIALE
011600010601     C                   if        invfil <> *zeros and
011700010601     C                             flgGiro = '0'
011800010601     C*
011900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012000010601     C                   eval      flgGiro = '1'
012100010601     C*
012200010601     C                   endif
012300010601     C*
012400010601     C                   ENDSR
012500010601     C***
012600010601
012700010601
012800010601
012900010601
013000010601     C*--------------------------------------------------------
013100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013200010601     C*--------------------------------------------------------
013300010601     C     ENDELA        BEGSR
013400040119     C*
013500040119     C* Effettuo la chiamata al CLLE preposto
013600040119     C                   call(e)   'TITVVTC'
013700040510     C                   parm                    parccm
013800040119     C                   parm                    parmbr
013900040119     C                   parm      '2'           paropz
014000050201     C*
014100050201     C* Effettuo lancio TISI95 solo x chiusura
014200050201     C                   CLEAR                   TISI95DS
014300050201     C                   EVAL      I95TLA = 'C'
014400050201     C                   CALL      'TISI95R'
014500050201     C                   PARM                    TISI95DS
014600000616     C*
014700010601     C                   ENDSR
014800010601     C***
014900000613
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
017700070823     C*
017800070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017900070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018000160211     C                   exsr      inzvar
018100160211     C                   exsr      impvab
018200010330     C*
018300030822     C                   do        *HIVAL
018400010330     C*
018500030822     C                   read      tivin00r                               70
018600010618     C*
018700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018800010618     C                   if        vinflg = '1'
018900010618     C                   eval      flgOk = '1'
019000010618     C                   endif
019100040510     C*
019200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019300040510     C                   z-add     *zeros        wLenVINDTA        4 0
019400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019800040510     C                   z-add     1             wFlgVINDTA
019900040510     C                   leave
020000040510     C                   endif
020100040510     C                   enddo
020200010618     C*
020300040510     C                   if        vindta > *blanks AND
020400040510     C                             wFlgVINDTA = 1
020500010330     C                   add       1             rrnum
020600010330     C*
020700010601     C                   if        *in70 = *off and
020800010330     C                             (vinflg = *blanks
020900010330     C                              or vinflg = '0'
021000010330     C                              or vinflg = '2')
021100010330     C*
021200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021300010711     C                   if        vinflg = *blanks or vinflg = '0'
021400010711     C                   clear                   vinmsg
021500010711     C                   endif
021600010601     C*
021700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021800070925     C                   eval      vindta  = ' ' + %trim(vindta)
021900070925     C*
022000080527     C                   exsr      inzvar
022100100318     C                   exsr      normCSV
022200010605     C                   exsr      impvab
022300010601     C*
022400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022500010605     C                   eval      depfil = VABLNP
022600010601     C                   exsr      repfil
022700010601     C                   if        depfil = invfil
022800021025     C                   if        vlrpoi = 999
022900030822     C                   move(P)   invfil        VABFGS
023000021025     C                   else
023100030822     C                   move(P)   vlrpoi        VABFGS
023200021025     C                   endif
023300010601     C*
023400010601     C                   exsr      PREELA
023500010601     C*
023600010604     C* Ebbene...
023700010604     C*
023800040119     C  N31              add       1             §CTROKVB
023900040119     C   32              add       1             §CTRMO
024000040119     C   31              add       1             §CTRNO
024100080418     C*
024200030822     C                   if        wGiro = 2
024300080513     C* Scrivo sempre il VAT
024400080513     C  N31              exsr      wrivat
024500080527     C                   exsr      chkvab
024600080513     C                   endif
024700010604     C*
024800010604     C                   if        *in31 = *off and
024900010604     C                             *in32 = *off
025000010604     C                   eval      vinflg = '1'
025100010604     C                   else
025200010604     C                   eval      vinflg = '2'
025300010604     C                   endif
025400010604     C                   endif
025500010601     C*
025600010604     C                   endif
025700010604     C*
025800010330     C                   else
025900010330     C                   eval      vinflg = '1'
026000010330     C                   endif
026100010601     C*
026200010601     C  N70              update    tivin000
026300010330     C*
026400030822     C  N70              enddo
026500040119     C*
026600080527     C* Verifico dati rimasti "in cannna"...
026700080527     C                   seton                                        65
026800080527     C                   exsr      chkvab
026900080527     C*
027000040119     C                   endif                                                  (endif - chkcall)
027100010601     C*
027200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027300010601     C                   if        cntNonEl = *zeros or
027400010601     C                             flgMulti = '0'
027500010330     C* Se non ci sono record con errori ...
027600010601     C                   if        §ctrno = 0 and
027700010604     C                             §ctrmo = 0 and
027800010601     C                             flgStato <> '2'
027900010330     C* ... restituisco esito OK.
028000010330     C                   eval      wrkesito = '0'
028100010330     C                   else
028200040119     C                   if        §ctrokvb > 0
028300010330     C                   eval      wrkesito = '1'
028400010330     C                   else
028500010615     C                   if        flgOk = '0'
028600010615     C                   eval      wrkesito = '2'
028700010615     C                   else
028800010615     C                   eval      wrkesito = '6'
028900010615     C                   endif
029000010330     C                   endif
029100010330     C                   endif
029200010601     C                   else
029300010601     C                   eval      wrkesito = '9'
029400010601     C                   endif
029500010330     C*
029600010330     C                   if        %open(tivin00r)
029700010330     C                   close     tivin00r
029800010330     C                   endif
029900021025     C                   if        %open(fivabwwr)
030000021025     C                   close     fivabwwr
030100010330     C                   endif
030200040512     C                   if        %open(fivatwwr)
030300040512     C                   close     fivatwwr
030400040512     C                   endif
030500010601     C*
030600010601     C                   if        vlrpoi <> 999
030700010601     C                   eval      invfil = vlrpoi
030800010601     C                   endif
030900010330     C*
031000040119     C                   if        §ctrokvb > 0
031100010601     C                             and invfil > *zeros
031200010330     C                   exsr      invio
031300010330     C                   endif
031400010601     C*
031500010618     C                   if        flgGiro = '1'
031600010601     C                   exsr      endela
031700010618     C                   endif
031800010330     C*
031900010330     C                   ENDSR
032000010330     C***
032100080527
032200080527
032300080527
032400080527     C*----------------------------------------------------*
032500080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032600080527     C*----------------------------------------------------*
032700100413     C     CHKVAB        BEGSR
032800080527     C*
032900080527     C* Scrivo il VAB solo a rottura d spedizione
033000080527     C                   if        depspe = *zeros
033100080527     C  N31              eval      fivabds_sav = fivabds
033200080527     C                   eval      depspe = vabNSP
033300080527     C                   else
033400080527     C                   if        depspe = vabNSP AND *in65 = *off
033500100413     C   51              add       VABNCL        sav_VABNCL
033600100413     C   51              add       VABPKB        sav_VABPKB
033700100413     C   51              add       VABVLB        sav_VABVLB
033800100413     C  N51              z-add     VABNCL        sav_VABNCL
033900100413     C  N51              z-add     VABPKB        sav_VABPKB
034000100413     C  N51              z-add     VABVLB        sav_VABVLB
034100080527     C                   else
034200080527     C  N31              eval      fivabds_ok = fivabds_sav
034300080527     C  N31              exsr      wrivab
034400080527     C  N31              eval      fivabds_sav = fivabds
034500080527     C                   eval      depspe = vabNSP
034600080527     C                   endif
034700080527     C                   endif
034800080527     C*
034900080527     C                   ENDSR
035000040119
035100040119
035200040119
035300040119     C*----------------------------------------------------*
035400040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035500040119     C*----------------------------------------------------*
035600040510     C     WRIVAB        BEGSR
035700080513     C*
035800080513     C* Gestisco l'eventuale rottura x numero spedizione
035900080527     C                   if        VABNSP <> depspe or *in65 = *on
036000070301     C                   if        wVATNOT_E <> *blanks
036100070301     C                   eval      VABCTM = '7Q'
036200070301     C                   endif
036300100413     C                   write     fivab000
036400080513     C                   endif
036500040119     C*
036600040119     C                   ENDSR
036700040119
036800040119
036900040119
037000040119     C*----------------------------------------------------*
037100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037200040119     C*----------------------------------------------------*
037300040510     C     WRIVAT        BEGSR
037400060512     C*
037500060512     C* Inizializzo i campi chiave
037600060512     C                   eval      VATFGS = VABFGS
037700060512     C                   eval      VATAAS = VABAAS
037800060512     C                   eval      VATCCM = VABCCM
037900070823     C                   eval      VATNRS = VABNRS
038000060512     C                   eval      VATNSP = VABNSP
038100060512     C                   eval      VATLNP = VABLNP
038200040714     C*
038300040714     C* Solo se VATNOT valorizzato scarico il buffer
038400040714     C                   if        VATNOT <> *blanks
038500040119     C*
038600040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038700040510     C                   if        VATTRC = *blanks
038800040510     C                   eval      VATTRC = 'E'
038900040510     C                   endif
039000040510     C                   write     FIVAT000
039100040119     C                   add       1             §CTROKVT
039200040714     C*
039300040714     C                   endif
039400060331     C*
039500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039600060331     C                   if        wVATNOT_A <> *blanks
039700060331     C                   eval      VATNOT = wVATNOT_A
039800060331     C                   eval      VATTRC = 'A'
039900060331     C                   write     FIVAT000
040000060331     C                   add       1             §CTROKVT
040100060331     C                   endif
040200060331     C*
040300060331     C                   if        wVATNOT_B <> *blanks
040400060331     C                   eval      VATNOT = wVATNOT_B
040500060331     C                   eval      VATTRC = 'B'
040600060331     C                   write     FIVAT000
040700060331     C                   add       1             §CTROKVT
040800060331     C                   endif
040900060331     C*
041000060331     C                   if        wVATNOT_E <> *blanks
041100060331     C                   eval      VATNOT = wVATNOT_E
041200060331     C                   eval      VATTRC = 'E'
041300060331     C                   write     FIVAT000
041400060331     C                   add       1             §CTROKVT
041500060331     C                   endif
041600130204     C*
041700130204     C                   if        wVATNOT_I <> *blanks
041800130204     C                   eval      VATNOT = wVATNOT_I
041900130204     C                   eval      VATTRC = 'I'
042000130204     C                   write     FIVAT000
042100130204     C                   add       1             §CTROKVT
042200130204     C                   endif
042300130204     C*
042400130204     C                   if        wVATNOT_J <> *blanks
042500130204     C                   eval      VATNOT = wVATNOT_J
042600130204     C                   eval      VATTRC = 'J'
042700130204     C                   write     FIVAT000
042800130204     C                   add       1             §CTROKVT
042900130204     C                   endif
043000160211     C*
043100160211     C                   if        wVATNOT_S <> *blanks
043200160211     C                   eval      VATNOT = wVATNOT_S
043300160211     C                   eval      VATTRC = 'S'
043400160211     C                   write     FIVAT000
043500160211     C                   add       1             §CTROKVT
043600160211     C                   endif
043700040119     C*
043800040119     C                   ENDSR
043900040119
044000040119
044100040119
044200040119     C*----------------------------------------------------*
044300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044400040119     C*----------------------------------------------------*
044500040119     C     PREVAT        BEGSR
044600040119     C*
044700040119     C* Compongo il nome del membro da dare al FIVATWWR
044800040119     C                   eval      parmbr = vlrhdl
044900040119     C                   movel     'M'           parmbr
045000040510     C                   eval      parccm = vlrksc
045100040119     C                   eval      paropz = '1'
045200040119     C* Effettuo la chiamata al CLLE preposto
045300040119     C                   call(e)   'TITVVTC'
045400040119     C                   parm                    parccm
045500040119     C                   parm                    parmbr
045600040119     C                   parm                    paropz
045700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045800040119     C                   if        %error
045900040119     C                   movel     '1'           chkcall
046000040119     C                   else
046100040119     C                   movel     '0'           chkcall
046200040119     C                   endif
046300040119     C*
046400040119     C                   ENDSR
046500010601
046600010601
046700010601
046800010330     C*----------------------------------------------------*
046900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047000010330     C*----------------------------------------------------*
047100040119     C     INZVAR        BEGSR
047200080527     C*
047300080527     C                   setoff                                       65
047400010330     C*
047500030822     C* Inizializzo variabili di wrk
047600020204     C                   Z-ADD     *zeros        Num5_0            5 0
047700100705     C                   MOVEL     *blanks       wFlgCAS           1
047800060331     C                   MOVEL     *blanks       wVATNOT_A
047900060331     C                   MOVEL     *blanks       wVATNOT_B
048000060331     C                   MOVEL     *blanks       wVATNOT_E
048100130204     C                   MOVEL     *blanks       wVATNOT_I
048200130204     C                   MOVEL     *blanks       wVATNOT_J
048300130204     C                   MOVEL     *blanks       wVATNOT_IJ
048400160211     C                   MOVEL     *blanks       wVATNOT_S
048500030822     C*
048600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048700030822     C                   CLEAR                   SkSplitCSV
048800030822     C                   EVAL      i = 1
048900030822     C                   EVAL      posDa = *zeros
049000030822     C                   EVAL      posA  = *zeros
049100080527     C*
049200080527     C                   CLEAR                   FIVABDS
049300080527     C                   CLEAR                   FIVAT000
049400030822     C*
049500030822     C* Reimposto i valori di default
049600030822     C                   EXSR      DEFCAM
049700030822     C*
049800010330     C                   ENDSR
049900010330     C*----------------------------------------------------*
050000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050100010330     C*----------------------------------------------------*
050200010330     C     DEFCAM        BEGSR
050300030822     C*
050400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050500030715     C* e delimitatore testo.
050600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050900100318     C*
051000100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051100100318     C                   EVAL      CharSOS = CharNUM
051200030822     C*
051300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051400100413     C                   SETOFF                                       5051
051500100413     C                   SELECT
051600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051700030715     C                   SETON                                        50
051800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051900100413     C                   SETON                                        51
052000100413     C                   ENDSL
052100030822     C*
052200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052300030822     C                   EVAL      posDaDft = 1
052400030822     C                   EVAL      posADft  = 0
052500030822     C                   EVAL      wGiroDft = 0
052600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052700030822     C                             posDaDft > 0
052800030822     C*
052900030822     C* Gestisco il 1° giro
053000030822     C                   IF        wGiroDft = 0
053100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053300030822     C* Incremento il contatore dei "giri"
053400030822     C                   EVAL      wGiroDft = 1
053500030822     C                   ELSE
053600030822     C                   EVAL      posDaDft = posADft
053700030822     C                   ENDIF
053800030822     C* Eseguo lo scan x trovare la fine del campo corrente
053900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054000030822     C*
054100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054300030822     C                   IF        posDaDft > 0 AND
054400030822     C                             posADft  > 0
054500030822     C* NCL
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'NCL'
055000030822     C                   EVAL      PiStr=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   EXSR      CHKNUM
055400030822     C                   IF        PiInt=*on
055500030822     C                   Z-ADD     PiVal         VABNCL
055600030822     C                   ENDIF
055700030822     C                   ENDIF
055800030822     C* CCM
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'CCM'
056300030822     C                   EVAL      PiStr=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   EXSR      CHKNUM
056700030822     C                   IF        PiInt=*on
056800030822     C                   Z-ADD     PiVal         VABCCM
056900030822     C                   ENDIF
057000030822     C                   ENDIF
057100030822     C* LNP
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'LNP'
057600030822     C                   EVAL      PiStr=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   EXSR      CHKNUM
058000030822     C                   IF        PiInt=*on
058100030822     C                   Z-ADD     PiVal         VABLNP
058200030822     C                   ENDIF
058300030822     C                   ENDIF
058400030822     C* NRS
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'NRS'
058900030822     C                   EVAL      PiStr=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   EXSR      CHKNUM
059300030822     C                   IF        PiInt=*on
059400030822     C                   Z-ADD     PiVal         VABNRS
059500030822     C                   ENDIF
059600030822     C                   ENDIF
059700030822     C* CTR
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'CTR'
060200030822     C                   EVAL      PiStr=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   EXSR      CHKNUM
060600030822     C                   IF        PiInt=*on
060700030822     C                   Z-ADD     PiVal         VABCTR
060800030822     C                   ENDIF
060900030822     C                   ENDIF
061000030822     C* PKB
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'PKB'
061500030822     C                   EVAL      PiStr=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   EXSR      CHKNUM
061900030822     C                   IF        PiNum=*on
062000030822     C                   Z-ADD     PiVal         VABPKB
062100030822     C                   ENDIF
062200030822     C                   ENDIF
062300030822     C* VLB
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'VLB'
062800030822     C                   EVAL      PiStr=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   EXSR      CHKNUM
063200030822     C                   IF        PiNum=*on
063300030822     C                   Z-ADD     PiVal         VABVLB
063400030822     C                   ENDIF
063500030822     C                   ENDIF
063600030822     C* QFT
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'QFT'
064100030822     C                   EVAL      PiStr=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   EXSR      CHKNUM
064500030822     C                   IF        PiNum=*on
064600030822     C                   Z-ADD     PiVal         VABQFT
064700030822     C                   ENDIF
064800030822     C                   ENDIF
064900030822     C* CBO
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'CBO'
065400030822     C                   EVAL      VABCBO=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   ENDIF
065800030822     C* TSP
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'TSP'
066300030822     C                   EVAL      VABTSP=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* VAS
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'VAS'
067200030822     C                   EVAL      VABVAS=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* VCA
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'VCA'
068100030822     C                   EVAL      VABVCA=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500070925     C* TIC
068600070925     C                   IF        %subst(
068700070925     C                             %subst(vlrppt:posDaDft+1:
068800070925     C                             posADft-posDaDft-1):1:3)
068900070925     C                             = 'TIC'
069000070925     C                   EVAL      VABTIC=%trim(%subst(
069100070925     C                             %subst(vlrppt:posDaDft+1:
069200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300070925     C                   ENDIF
069400030822     C* GCA
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'GCA'
069900030822     C                   EVAL      VABGCA=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* CTM
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'CTM'
070800030822     C                   EVAL      VABCTM=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* FFD
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'FFD'
071700030822     C                   EVAL      VABFFD=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* VAD
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'VAD'
072600030822     C                   EVAL      VABVAD=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* GMA
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'GMA'
073500030822     C                   EVAL      VABGMA=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* GGA
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'GGA'
074400030822     C                   EVAL      VABGGA=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* GVA
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'GVA'
075300030822     C                   EVAL      VABGVA=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* TC1
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'TC1'
076200030822     C                   EVAL      VABTC1=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* TC2
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'TC2'
077100030822     C                   EVAL      VABTC2=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500040714     C* VATTRC
077600040714     C                   IF        %subst(
077700040714     C                             %subst(vlrppt:posDaDft+1:
077800040714     C                             posADft-posDaDft-1):1:3)
077900040714     C                             = 'TRC'
078000040714     C                   EVAL      VATTRC=%trim(%subst(
078100040714     C                             %subst(vlrppt:posDaDft+1:
078200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300040714     C                   ENDIF
078400030822     C* ...
078500030822     C                   ENDIF
078600030822     C                   ENDDO
078700020204     C*
078800010330     C                   ENDSR
078900010607     C*----------------------------------------------------*
079000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079100010607     C*----------------------------------------------------*
079200010607     C     IMPVAB        BEGSR
079300010607     C*
079400010607     C                   SETOFF                                       3132
079500030515     C*
079600010607     C                   MOVEL     datcor        VABAAS
079700010607     C                   MOVE      datcor        VABMGS
079800010607     C*
079900030715     C**********
080000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080100030715     C**********
080200030715     C                   IF        wGiro = *zeros
080300040113     C*
080400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080600080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080700080429     C                             %trim(intcol(5))<>*blanks
080800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080900080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081000080429     C                                      %trim(intcol(5))
081100070823     C     ';':CharCSV   XLATE     vindta        vindta
081200040510     C                   ENDIF
081300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081400031201     C     minu:maiu     XLATE     vindta        vindta
081500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081600030715     C                   DOW       posDa <= %len(%trim(vindta))
081700030715     C*
081800030715     C* Gestisco il 1° campo
081900030715     C                   IF        i = 1
082000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082100030715     C                   EVAL      posDa = 1
082200030715     C* Eseguo lo scan x trovare la fine del primo campo
082300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082400030715     C* A questo punto "estraggo" il campo corrente
082500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082600030715     C                                                    (posA-posDa))
082700030715     C* X i campi successivi al 1°
082800030715     C                   ELSE
082900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083000030715     C                   EVAL      posDa = posA + 1
083100030715     C* Eseguo lo scan x trovare la fine del campo corrente
083200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083300030715     C* Gestisco l'ultimo campo
083400030715     C                   IF        posA = *zeros
083500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083600030715     C* A questo punto "estraggo" il campo corrente
083700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083800030715     C                                                    (posA-posDa))
083900030715     C                   LEAVE
084000030715     C                   ELSE
084100030715     C* A questo punto "estraggo" il campo corrente
084200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084300030715     C                                                    (posA-posDa))
084400030715     C                   ENDIF
084500030715     C                   ENDIF
084600030715     C* Incremento il contatore d campo
084700030715     C                   EVAL      i = i +1
084800030715     C                   ENDDO
084900030715     C*
085000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085100030715     C* delimitazione testo
085200030715     C                   EVAL      i = 1
085300030715     C                   DOW       i < %elem(SkSplitFLD)
085400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085600030715     C                   EVAL      i = i + 1
085700030715     C                   ENDDO
085800030715     C                   EVAL      wGiro = 1
085900030715     C                   ELSE
086000030715     C                   EVAL      wGiro = 2
086100031204     C**********
086200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086300031204     C* d separatore campo
086400031204     C**********
086500031204     C                   EVAL      vindta = %trim(vindta)
086600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086700031204     C                   EVAL      vindta = %subst(vindta:2)
086800031204     C                   ENDDO
086900031204     C*
087000031204     C                   Z-ADD     *zeros        lunghInput        4 0
087100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087500031204     C                   ENDDO
087600030715     C**********
087700030715     C* Effettuo lo split del campo dati d input
087800030715     C**********
087900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088000030715     C                   DOW       posDa <= %len(%trim(vindta))
088100030715     C*
088200030715     C* Gestisco il 1° campo
088300030715     C                   IF        i = 1
088400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088500030715     C                   EVAL      posDa = 1
088600030715     C* Eseguo lo scan x trovare la fine del primo campo
088700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C* X i campi successivi al 1°
089200030715     C                   ELSE
089300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089400030715     C                   EVAL      posDa = posA + 1
089500031204     C* Verifico che nn vi sia il campo nn valorizzato
089600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089700031204     C* Se campo nn valorizzato skippo al prossimo
089800031204     C                   EVAL      SkSplitCSV(i) = *blanks
089900031204     C                   EVAL      posA  = posA + 1
090000031204     C                   ELSE
090100030715     C* Eseguo lo scan x trovare la fine del campo corrente
090200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090300030715     C* Gestisco l'ultimo campo
090400030715     C                   IF        posA = *zeros
090500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090600030715     C* A questo punto "estraggo" il campo corrente
090700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090800030715     C                                                    (posA-posDa))
090900030715     C                   LEAVE
091000030715     C                   ELSE
091100030715     C* A questo punto "estraggo" il campo corrente
091200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091300030715     C                                                    (posA-posDa))
091400030715     C                   ENDIF
091500031204     C                   ENDIF
091600030715     C                   ENDIF
091700030715     C* Incremento il contatore d campo
091800030715     C                   EVAL      i = i +1
091900030715     C                   ENDDO
092000030715     C*
092100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092200030715     C* delimitazione testo
092300030715     C                   EVAL      i = 1
092400030715     C                   DOW       i < %elem(SkSplitCSV)
092500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092700030715     C                   EVAL      i = i + 1
092800030715     C                   ENDDO
092900100326     C*
093000100326     C* Stacco un progressivo x numero spedizione
093100160211     C                   EXSR      repNSP
093200100326     C*
093300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093400030715     C                   EVAL      i = 1
093500030715     C                   DOW       i < %elem(SkSplitFLD)
093600030715     C*
093700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093900030715     C                             SkSplitCSV(i) <> *blanks
094000030721     C*** ATB
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094200030721     C                   EVAL      VABATB = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030715     C*** RSD
094500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094900030715     C* ==
095000030715     C                   ENDIF
095100030721     C*** RD2
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030715     C*** IND
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095700030715     C                   EVAL      VABIND = SkSplitCSV(i)
095800030715     C                   ENDIF
095900030715     C*** LOD
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096200030715     C                   ENDIF
096300030715     C*** PRD
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096600030715     C                   ENDIF
096700030721     C*** NZD
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030715     C*** RMA
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030715     C*** NOT
097600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097800030715     C                   ENDIF
097900030715     C*** NT2
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030715     C*** VCA
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** VAS
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** FFD
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030721     C*** GC1
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** GC2
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** TSP
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100600030721     C                   ENDIF
100700040510     C*** CBO
100800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101000040510     C                   ENDIF
101100030721     C*** NAS
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** TIC
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** GCA
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** XCO
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** CTM
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** TCR
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** CTS
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** FTM
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** VAD
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** GMA
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GGA
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** GVA
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** TC1
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** TC2
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** SCL
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** RMO
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** NMO
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*
108000030721     C* Reperisco quindi i campi numerici...
108100040510     C*** NSP
108200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108300040510     C                   EVAL      PiStr=SkSplitCSV(i)
108400040510     C                   EXSR      CHKNUM
108500040510     C                   IF        PiInt=*on
108600040510     C                   Z-ADD     PiVal         VABNSP
108700040510     C                   ELSE
108800040510     C                   SETON                                        31
108900040510     C                   EVAL      VABNSP = *zeros
109000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109100040510     C                             + ' ' + 'VABNSP'
109200040510     C                   ENDIF
109300040510     C                   ENDIF
109400071106     C*** RMN
109500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109600071106     C                   EVAL      PiStr=SkSplitCSV(i)
109700071106     C                   EXSR      CHKNUM
109800071106     C                   IF        PiInt=*on
109900071106     C                   Z-ADD     PiVal         VABRMN
110000071106     C                   ELSE
110100071106     C                   SETON                                        32
110200071106     C                   EVAL      VABRMN = *zeros
110300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110400071106     C                             + ' ' + 'VABRMN'
110500071106     C                   ENDIF
110600071106     C                   ENDIF
110700030721     C*** CAD
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110900030721     C                   EVAL      PiStr=SkSplitCSV(i)
111000030721     C                   EXSR      CHKNUM
111100030721     C                   IF        PiInt=*on
111200030721     C                   Z-ADD     PiVal         Num5_0
111300030721     C                   MOVEL(p)  Num5_0        VABCAD
111400030721     C                   ELSE
111500030721     C                   SETON                                        32
111600030721     C                   EVAL      VABCAD = *zeros
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800030721     C                             + ' ' + 'VABCAD'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** NCL
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiInt=*on
112600030721     C                   Z-ADD     PiVal         VABNCL
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900030721     C                   EVAL      VABNCL = *zeros
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100040122     C                             + ' ' + 'VABNCL'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** PKB
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiNum=*on
113900030721     C                   Z-ADD     PiVal         VABPKB
114000030721     C                   ELSE
114100030721     C                   SETON                                        32
114200030721     C                   EVAL      VABPKB = *zeros
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400030721     C                             + ' ' + 'VABPKB'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** CAS
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000100705     C                   IF        PiStr<>*blanks
115100100705     C                   EVAL      wFlgCAS = '1'
115200100705     C                   EVAL      VABVCA='EUR'
115300100705     C                   ENDIF
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiNum=*on
115600030721     C                   Z-ADD     PiVal         VABCAS
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      VABCAS = *zeros
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABCAS'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** CCM
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiInt=*on      AND
116900030721     C                             PiVal<=9999999 AND
117000030721     C                             PiVal>*zeros
117100030721     C                   Z-ADD     PiVal         VABCCM
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      VABCCM = *zeros
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600040510     C                             + ' ' + 'VABCCM'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** LNP
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiInt=*on    AND
118400030721     C                             PiVal<=999   AND
118500030721     C                             PiVal>*zeros
118600030721     C                   Z-ADD     PiVal         VABLNP
118700030721     C                   ELSE
118800040715     C                   SETON                                        32
118900030721     C                   EVAL      VABLNP = *zeros
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100040510     C                             + ' ' + 'VABLNP'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** NRS
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiInt=*on AND
119900030721     C                             PiVal<=99
120000030721     C                   Z-ADD     PiVal         VABNRS
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400030721     C                             + ' ' + 'VABNRS'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** LNA
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiInt=*on    AND
121200030721     C                             PiVal<=999   AND
121300030721     C                             PiVal>*zeros
121400030721     C                   Z-ADD     PiVal         VABLNA
121500030721     C                   ELSE
121600030721     C                   SETON                                        32
121700030721     C                   EVAL      VABLNA = *zeros
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABLNA'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** CTR
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiInt=*on  AND
122700030721     C                             PiVal<=999
122800030721     C                   Z-ADD     PiVal         VABCTR
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABCTR'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** IAS
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiNum=*on
124000030721     C                   Z-ADD     PiVal         VABIAS
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABIAS'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** VLB
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiNum=*on
125200030721     C                   Z-ADD     PiVal         VABVLB
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABVLB'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** QFT
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiNum=*on
126400030721     C                   Z-ADD     PiVal         VABQFT
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABQFT'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** NCD
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiInt=*on      AND
127600030721     C                             PiVal<=9999999
127700030721     C                   Z-ADD     PiVal         VABNCD
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABNCD'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** NCA
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on      AND
128900030721     C                             PiVal<=9999999
129000030721     C                   Z-ADD     PiVal         VABNCA
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABNCA'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** ZNC
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on AND
130200030721     C                             PiVal<=99
130300030721     C                   Z-ADD     PiVal         VABZNC
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABZNC'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** DCR
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on       AND
131500030721     C                             PiVal<=99999999
131600030721     C                   Z-ADD     PiVal         VABDCR
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABDCR'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** HCR
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on   AND
132800030721     C                             PiVal<=9999
132900030721     C                   Z-ADD     PiVal         VABHCR
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABHCR'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600030721     C*** VMD
133700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiNum=*on
134100030721     C                   Z-ADD     PiVal         VABVMD
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABVMD'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** ANT
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiInt=*on        AND
135300030721     C                             PiVal<=999999999
135400030721     C                   Z-ADD     PiVal         VABANT
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABANT'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** CMO
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiInt=*on
136600030721     C                   Z-ADD     PiVal         Num5_0
136700030721     C                   MOVEL(p)  Num5_0        VABCMO
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000030721     C                   EVAL      VABCMO = *zeros
137100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137200030721     C                             + ' ' + 'VABCMO'
137300030721     C                   ENDIF
137400030721     C                   ENDIF
137500040510     C*
137600040510     C* Reperisco i campi del FIVAT
137700040510     C*** VATNOT
137800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138000040510     C                   ENDIF
138100040510     C*** VATTRC
138200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138400040510     C                   ENDIF
138500060331     C*** VATNOT_A
138600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
138700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
138800060331     C                   ENDIF
138900060331     C*** VATNOT_B
139000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139200060331     C                   ENDIF
139300060331     C*** VATNOT_E
139400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
139500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139600060331     C                   ENDIF
139700070823     C*
139800071029     C***  ===>  Gestione campi "particolari" dentro ciclo
139900100413     C*** NZD/CTM/CTR
140000100413     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
140100100413     C                   EVAL      VABNZD=SkSplitCSV(i)
140200100705     C*
140300100705     C* ...passo dal codice ISO a quello "Bartolini"
140400100705     C                   Z-ADD     1             jNAZ
140500100705     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
140600100705     C                   IF        %found
140700100705     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
140800100705     C                   ENDIF
140900100705     C*
141000100413     C                   IF        VABNZD<> *blanks
141100100413     C                   EVAL      VABCTM='7R'
141200100413     C                   EVAL      VABCTR=352
141300100413     C                   ELSE
141400100413     C                   EVAL      VABCTM='7Q'
141500120918     C***                EVAL      VABCTR=000
141600120918     C                   EVAL      VABCTR=001
141700100413     C                   ENDIF
141800100326     C                   ENDIF
141900160211     C*** RMN / RMA
142000160211     C*  RMN è solo la parte numerica sx del dato
142100100705     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
142200160211     C                   EVAL      VABRMA=SkSplitCSV(i)
142300160211     C                   EVAL      PiStr=
142400160211     C                             %subst(SkSplitCSV(i):1:
142500160211     C                              %scan('/':SkSplitCSV(i)) - 1)
142600160211     C***                EVAL      PiStr=SkSplitCSV(i)
142700100705     C                   EXSR      CHKNUM
142800100705     C                   IF        PiInt=*on
142900100705     C                   Z-ADD     PiVal         VABRMN
143000160211     C***                Z-ADD     PiVal         VABNSP
143100100705     C                   ELSE
143200100705     C                   SETON                                        31
143300100705     C                   EVAL      VABRMN = *zeros
143400100705     C                   EVAL      vinmsg = %trimr(vinmsg)
143500160211     C                             + ' ' + 'VABRMN'
143600100705     C                   ENDIF
143700100705     C                   ENDIF
143800130204     C*
143900130204     C*** VATNOT_I/J
144000130204     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
144100160211     C* non va più valorizzato l'alert via email
144200160211     C***                EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
144300160211     C***                EVAL      wVATNOT_I=%subst(wVATNOT_IJ:1:35)
144400160211     C***                EVAL      wVATNOT_J=%subst(wVATNOT_IJ:36:35)
144500130204     C                   ENDIF
144600160211     C*
144700160211     C*** VATNOT_S (uguale al telefono per contatto)
144800160211     C                   EVAL      wVATNOT_S = wVATNOT_B
144900070925     C*
145000071029     C***  <===  -----------------------------------------
145100040510     C*
145200030715     C                   ENDIF
145300030715     C                   EVAL      i = i + 1
145400030715     C                   ENDDO
145500070925     C*
145600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145700070925     C*
145800071029     C***  <===  -----------------------------------------
145900070925     C*
146000070925     C* Considerazioni finali su CBO/CAS
146100100318     C                   IF        wFlgCAS = '1'    OR
146200100318     C                             VABCAS  > *zeros
146300070925     C                   IF        VABCBO = '1'
146400070925     C                   EVAL      VABCBO = '4'
146500070925     C                   ENDIF
146600070925     C                   IF        VABCBO = '2'
146700070925     C                   EVAL      VABCBO = '6'
146800070925     C                   ENDIF
146900070925     C                   ENDIF
147000070925     C***  <===  ----------------------------
147100040510     C*
147200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
147300040510     C                   IF        VABNSP = *zeros
147400040510     C                   SETON                                        31
147500040510     C                   ENDIF
147600040715     C*
147700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
147800040715     C                   IF        VABLNP = *zeros
147900040715     C                   SETON                                        31
148000040715     C                   ENDIF
148100040510     C*
148200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
148300040510     C                   IF        VABRMN = *zeros
148400040510     C                   Z-ADD     VABNSP        VABRMN
148500040510     C                   ENDIF
148600040510     C*
148700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
148800040510     C                   IF        VABRMA = *blanks
148900040714     C                   MOVEL     VABRMN        VABRMA
149000040510     C                   ENDIF
149100040128     C*
149200040510     C* Se provincia nn valorizzata la reperisco
149300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
149400040510     C                   IF        VABPRD = *blanks
149500040128     C                   CLEAR                   TISI95DS
149600040128     C                   EVAL      I95TCN = '3'
149700040128     C                   Z-ADD     datcor        I95DAT
149800040128     C                   EVAL      I95NAR = VABNZD
149900040128     C                   EVAL      I95CAP = VABCAD
150000040128     C                   EVAL      I95LOC = VABLOD
150100040128     C                   CALL      'TISI95R'
150200040128     C                   PARM                    TISI95DS
150300040128     C                   EVAL      VABPRD = O95PRV
150400040510     C                   ENDIF
150500020204     C*
150600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
150700020204     C                   EXSR      CHKIMPDIV
150800030715     C                   ENDIF
150900020204     C*
151000010607     C                   ENDSR
151100010607     C*----------------------------------------------------*
151200010601
151300020204
151400020204     C*----------------------------------------------------*
151500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
151600020204     C*----------------------------------------------------*
151700020204     C     CHKIMPDIV     BEGSR
151800020204     C*
151900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152000020204     C                   Z-ADD     *zeros        wrkDec            9 9
152100020204     C*
152200020204     C* Come prima cosa effettuo considerazioni sulla divisa
152300020204     C                   IF        vabIAS > *zeros
152400020204     C                   IF        vabVAS <> 'EUR'
152500020204     C                   EVAL      vabVAS =  'ITL'
152600020204     C                   ENDIF
152700020204     C                   ENDIF
152800020204     C*
152900020204     C                   IF        vabCAS > *zeros
153000020204     C                   IF        vabVCA <> 'EUR'
153100020204     C                   EVAL      vabVCA =  'ITL'
153200020204     C                   ENDIF
153300020204     C                   ENDIF
153400020204     C*
153500020204     C                   IF        vabVMD > *zeros
153600020204     C                   IF        vabVAD <> 'EUR'
153700020204     C                   EVAL      vabVAD =  'ITL'
153800020204     C                   ENDIF
153900020204     C                   ENDIF
154000020204     C*
154100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
154200020204     C                   Z-ADD     vabIAS        wrkDec
154300020204     C                   IF        wrkDec > *zeros
154400020204     C                   IF        vabVAS = 'ITL'
154500020204     C                   EVAL      vabIAS = *zeros
154600020204     C                   ENDIF
154700020204     C                   ENDIF
154800020204     C*
154900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155000020204     C                   Z-ADD     vabCAS        wrkDec
155100020204     C                   IF        wrkDec > *zeros
155200020204     C                   IF        vabVCA = 'ITL'
155300020204     C                   EVAL      vabCAS = *zeros
155400020204     C                   ENDIF
155500020204     C                   ENDIF
155600020204     C*
155700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
155800020204     C                   Z-ADD     vabVMD        wrkDec
155900020204     C                   IF        wrkDec > *zeros
156000020204     C                   IF        vabVAD = 'ITL'
156100020204     C                   EVAL      vabVMD = *zeros
156200020204     C                   ENDIF
156300020204     C                   ENDIF
156400020204     C*
156500020204     C                   ENDSR
156600020204     C***
156700010330
156800100318
156900100318
157000100318     C*----------------------------------------------------*
157100100318     C*  CONTROLLO NUMERICITA' CAMPI
157200100318     C*----------------------------------------------------*
157300100318     C     CHKNUM        BEGSR
157400100318     C*
157500100318     C                   IF        PiDecChr = *blanks
157600100318     C                   EVAL      PiDecChr = CharNUM
157700100318     C                   ENDIF
157800100318     C*
157900100318     C                   callp     UBISNUM_Check(PiStr
158000100318     C                                          :PiDecChr
158100100318     C                                          :PiVal
158200100318     C                                          :PiNum
158300100318     C                                          :PiInt)
158400100318     C*
158500100318     C                   ENDSR
158600100318     C***
158700010601
158800010601
158900010601
159000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159100010601     C     repfil        BEGSR
159200010601     C*
159300010601     C                   if        invfil = *zeros and
159400010601     C                             depfil > *zeros and
159500010629     C                             (vinflg = *blanks or
159600010629     C                              vinflg = *zeros)
159700010601     C
159800010601     C                   eval      invfil = depfil
159900010601     C                   endif
160000010601     C*
160100010601     C                   if        depfil <> invfil and
160200010601     C                             invfil > *zeros
160300010601     C                   eval      flgMulti = '1'
160400010601     C                   if        vinflg = *blanks
160500010601     C                   add       1             cntNonEl
160600010601     C                   endif
160700010601     C                   endif
160800010601     C*
160900010601     C                   if        vinflg = '2'
161000010601     C                   eval      flgStato = '2'
161100010601     C                   endif
161200010601     C*
161300010601     C                   ENDSR
161400010601     C***
161500010601
161600010601
161700010601
161800010330
161900040119      /TITLE Invio dei dati al punto operativo.
162000040119     C     invio         BEGSR
162100040119     C*
162200040119     C* 1° invio FIVAT
162300040119     C                   reset                   dscmz
162400040119     C                   move      vlrpoi        cmzdst
162500040119     C                   eval      cmzfld = 'FIVATWWR'
162600040119     C                   eval      cmzmbd = vlrhdl
162700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162800040119     C***                if        prmfir = *blanks
162900040119     C                   eval      cmzfla = 'FIVAT00F'
163000040119     C                   eval      cmzmba = 'FIVAT00F'
163100040119     C***                else
163200040119     C***                eval      cmzfla = prmfir
163300040119     C***                eval      cmzmba = prmfir
163400040119     C***                endif
163500040119     C                   eval      cmznrr = *zeros
163600040119     C                   move      §ctrokvt      cmznrr
163700040119     C                   eval      cmzlba = vlrfl1
163800040119     C                   call(e)   'TIS711C'
163900040119     C                   parm                    dscmz
164000040119     C                   parm      *blanks       esito
164100040119     C                   if        %error
164200040119     C                             or cmzerr = '1'
164300040119     C                             or esito  = '1'
164400040119     C                   eval      wrkesito = '3'
164500040119     C                   else
164600040119     C*
164700040119     C* 2° invio FIVAB
164800040119     C                   reset                   dscmz
164900040119     C                   move      vlrpoi        cmzdst
165000040119     C                   eval      cmzfld = vlrfou
165100040119     C                   eval      cmzmbd = vlrhdl
165200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165300040119     C***                if        prmfir = *blanks
165400040119     C                   eval      cmzfla = 'FIVAB00F'
165500040119     C                   eval      cmzmba = 'FIVAB00F'
165600040119     C***                else
165700040119     C***                eval      cmzfla = prmfir
165800040119     C***                eval      cmzmba = prmfir
165900040119     C***                endif
166000040119     C                   eval      cmznrr = *zeros
166100040119     C                   move      §ctrokvb      cmznrr
166200040119     C                   eval      cmzlba = vlrfl1
166300040119     C                   call(e)   'TIS711C'
166400040119     C                   parm                    dscmz
166500040119     C                   parm      *blanks       esito
166600040119     C                   if        %error
166700040119     C                             or cmzerr = '1'
166800040119     C                             or esito  = '1'
166900040119     C                   eval      wrkesito = '3'
167000040119     C                   endif
167100040119     C                   endif
167200040119     C*
167300040119     C                   ENDSR
167400040119     C***
167500010601
167600010601
167700010601
167800010601
167900010601      /TITLE Invio dei dati al punto operativo.
168000010601     C     opeini        BEGSR
168100010601     C*
168200010601     C* Inizializzo flag e contatori operativi
168300010601     C                   movel     '0'           flgGiro           1
168400010601     C                   movel     '0'           flgMulti          1
168500010601     C                   movel     '1'           flgStato          1
168600010615     C                   movel     '0'           flgOk             1
168700010601     C                   z-add     *zeros        cntNonEl         10 0
168800010601     C                   z-add     *zeros        depfil            3 0
168900010601     C                   z-add     *zeros        invfil            3 0
169000010601     C*
169100010601     C                   ENDSR
169200010601     C***
169300070326
169400070326
169500070326
169600070326
169700070326     C     *pssr         BEGSR
169800070329     C*
169900070329     C                   if        %open(tivin00r)
170000070329     C                   close     tivin00r
170100070329     C                   endif
170200070329     C                   if        %open(fivabwwr)
170300070329     C                   close     fivabwwr
170400070329     C                   endif
170500070329     C                   if        %open(fivatwwr)
170600070329     C                   close     fivatwwr
170700070329     C                   endif
170800070326     C*
170900070326     C* Effettuo la chiamata al CLLE preposto
171000070326     C                   call(e)   'TITVVTC'
171100070326     C                   parm                    parccm
171200070326     C                   parm                    parmbr
171300070326     C                   parm      '2'           paropz
171400070326     C*
171500070326     C                   eval      wrkesito = '2'
171600070404     C*
171700070404     C                   seton                                        LR
171800070326     C*
171900070326     C                   ENDSR     '*CANCL'
172000070326     C***
172100100318
172200100318
172300100318
172400100318     C     normCSV       BEGSR
172500100318     C*
172600100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
172700100318     C* separatori campi
172800100318     C                   if        CharTXT <> *blanks
172900100318     C                   z-add     *zeros        wLenVINDTA        4 0
173000100318     C                   z-add     *zeros        wFlgVINDTA        1 0
173100100318     C                   setoff                                       80
173200100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
173300100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
173400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
173500100318     C                   if        not *in80
173600100318     C                   seton                                        80
173700100318     C                   else
173800100318     C                   setoff                                       80
173900100318     C                   endif
174000100318     C                   endif
174100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
174200100318     C                             and *in80 = *on
174300100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
174400100318     C                   endif
174500100318     C                   enddo
174600100318     C                   endif
174700100318     C*
174800100318     C                   ENDSR
174900100318     C***
175000070326
175100100326
175200100326
175300100326     C*----------------------------------------------------*
175400100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
175500100326     C*----------------------------------------------------*
175600100326     C     REPNSP        BEGSR
175700100326     C*
175800100326     C* NSP => Stacco un numeratore da AZNUM
175900100326     C                   clear                   TRUL33DS
176000100326     C                   eval      I33OPE = *zeros
176100100326     C                   eval      I33CNU = 302
176200100326     C                   eval      I33NUM = 1
176300100326     C                   movel     TRUL33DS      KPJBU
176400100326     C                   call      'TRUL33R'
176500100326     C                   parm                    KPJBA
176600100326     C                   movel     KPJBU         TRUL33DS
176700100326     C                   if        O33ERR = *zeros
176800100326     C                   z-add     O33NRF        VABNSP
176900100326     C                   z-add     O33NRF        VATNSP
177000100326     C                   else
177100100326     C                   SETON                                        31
177200100326     C                   EVAL      vinmsg = %trimr(vinmsg)
177300100326     C                             + ' ' + 'VABNSP VATNSP'
177400100326     C                   endif
177500100326     C*
177600100326     C                   ENDSR
177700010330
177800100705
177900100705
178000100705     C*--------------------------------------------------------
178100100705     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
178200100705     C*--------------------------------------------------------
178300100705     C     CARTAB        BEGSR
178400100705     C*
178500100705     C* TABELLA '15' - NAZIONI
178600100705     C                   clear                   skNAZISO
178700100705     C                   clear                   skNAZBAR
178800100705     C                   eval      tblKUT = 1
178900100705     C                   eval      tblCOD = '15'
179000100705     C     KEYtabP       setll     tabel00f
179100100705     C     KEYtabP       reade     tabel00f
179200100705     C                   dow       not %eof(tabel00f)
179300100705     C                   if        tblFLG = *blanks
179400100705     C                   movel(p)  tblUNI        ds15
179500100705     C                   add       1             jNAZ
179600100705     C                   eval      skNAZBAR(jNAZ) = tblKEY
179700100705     C                   eval      skNAZISO(jNAZ) = §15COD
179800100705     C                   endif
179900100705     C     KEYtabP       reade     tabel00f
180000100705     C                   enddo
180100100705     C*
180200100705     C                   ENDSR
180300100705     C***
180400100705
180500100326
180600010330
180700000613     C     *inzsr        BEGSR
180800990910     C*
180900990910     C     *entry        plist
181000990920     C                   parm                    tivlrds
181100990921     C                   parm      wrkesito      esito
181200000724     C                   parm                    prmlit
181300000710     C                   parm                    prmfir
181400100705     C*
181500100705     C* Chiave su TABEL00F - parziale
181600100705     C     KEYtabP       KLIST
181700100705     C                   KFLD                    tblKUT
181800100705     C                   KFLD                    tblCOD
181900010330     C*
182000010330     C* CALCOLA LA DATA CORRENTE
182100100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
182200100318     C                   eval      datcor = %dec(%date() : *ISO)
182300000613     C*
182400000613     C                   ENDSR
182500000613     C***
182600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182700100705W_VABRMN;VATNOT_E;VABRSD;VABRD2;VABIND;VABNOT;VABCAD;VABLOD;VABPRD;
182800130204W_VABNZD;VATNOT_B;VABNCL;VABPKB;VABCAS;VABCBO;VABFFD;VABNT2;W_VATNOT_I;VABIAS;
182900100318
183000100318
183100100318
