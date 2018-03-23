000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600040119     FFIVATwwr  O    E             DISK    usropn
000700000313     D*
000800040113     D*------------
000900040113     D* SCHIEDE A PROGRAMMA
001000040113     D*------------
001100040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900040128     D tisi95ds      e ds
002000010330     D esito           s              1
002100010330     D prmlit          s             10
002200010330     D prmfir          s             10
002300010330     D wrkesito        s                   like(esito)
002400010330     D rrnum           s              6  0 INZ(*zeros)
002500040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
002600040510     D parccm          s              8    INZ(*blanks)
002700040119     D parmbr          s             10    INZ(*blanks)
002800040119     D paropz          s              1    INZ(*blanks)
002900040119     D chkcall         s              1    INZ(*blanks)
003000080125     D wFlgCAS         s              1    INZ(*blanks)
003100080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003200030715     D*------------------
003300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003400030715     D*------------------
003500030715     D SkSplitFLD      S             10    DIM(100)
003600030715     D SkSplitCSV      S            256    DIM(100)
003700030715     D CharCSV         S              1
003800030715     D CharTXT         S              1
003900030715     D CharNUM         S              1
004000030715     D posDa           S              3  0 INZ(*zeros)
004100030715     D posA            S              3  0 INZ(*zeros)
004200030715     D i               s              3  0 INZ(1)
004300030715     D wGiro           s              1  0 INZ(*zeros)
004400080125     D wVATNOT_A       s                   LIKE(VATNOT)
004500080125     D wVATNOT_B       s                   LIKE(VATNOT)
004600080125     D wVATNOT_E       s                   LIKE(VATNOT)
004700030822     D*------------------
004800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004900030822     D*------------------
005000030822     D posDaDft        S              3  0 INZ(*zeros)
005100030822     D posADft         S              3  0 INZ(*zeros)
005200030822     D j               s              3  0 INZ(1)
005300030822     D wGiroDft        s              1  0 INZ(*zeros)
005400010330     D*------------------
005500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005600010330     D*------------------
005700010330     D WLBDA8          DS                  INZ
005800010330     D  G08DAT                 1      8  0
005900010330     D  G08INV                 9     16  0
006000010330     D  G08ERR                17     17
006100010330     D  G08TGI                18     22  0
006200031201     D*------------------
006300031201     D* Costanti
006400031201     D*------------------
006500031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006600031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006700080617     D*------------------
006800090921     D* DS REPERIMENTO NUMERATORE
006900090921     D*------------------
007000090921     D trul33ds      e ds                  inz
007100090921     D*------------------
007200090921     D* DS ARCHITETTURA
007300090921     D*------------------
007400090921     D kpjba         e ds                  inz
007500090921     D*------------------
007600100908
007700100908     D*------------------
007800100908     D* DS RIDEFINIZIONE RAGIONE SOCIALE DESTINATARIO
007900100908     D*------------------
008000100908     D W_RSD           DS                  INZ
008100100908     D  W_RSD_1                      35
008200100908     D  W_RSD_2                      35
008300100908     D  W_RSD_3                      35
008400100908     D  W_RSD_4                      35
008500080617
008600080617     D*------------------
008700080617     D* LINKING A DEFINIZIONI ESTERNE
008800080617     D*------------------
008900080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009000090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009100010330
009200010330
009300010330
009400000913     C                   reset                   rrnum
009500990921     C                   reset                   esito
009600081014     C                   reset                   wrkesito
009700010601     C*
009800010601     C                   exsr      opeini
009900010605     C                   exsr      rwvab
010000081014     C*
010100081014     C                   exsr      endela
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
013100081014     C*
013200081014     C                   if        %open(tivin00r)
013300081014     C                   close     tivin00r
013400081014     C                   endif
013500081014     C                   if        %open(fivabwwr)
013600081014     C                   close     fivabwwr
013700081014     C                   endif
013800081014     C                   if        %open(fivatwwr)
013900081014     C                   close     fivatwwr
014000081014     C                   endif
014100040119     C*
014200040119     C* Effettuo la chiamata al CLLE preposto
014300040119     C                   call(e)   'TITVVTC'
014400040510     C                   parm                    parccm
014500040119     C                   parm                    parmbr
014600040119     C                   parm      '2'           paropz
014700050201     C*
014800050201     C* Effettuo lancio TISI95 solo x chiusura
014900050201     C                   CLEAR                   TISI95DS
015000050201     C                   EVAL      I95TLA = 'C'
015100050201     C                   CALL      'TISI95R'
015200050201     C                   PARM                    TISI95DS
015300000613     C*
015400010601     C                   ENDSR
015500010601     C***
015600000613
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
018400070823     C*
018500070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018600070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018700090420     C                   exsr      inzvar
018800090420     C                   exsr      impvab
018900010330     C*
019000030822     C                   do        *HIVAL
019100010330     C*
019200030822     C                   read      tivin00r                               70
019300010618     C*
019400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019500010618     C                   if        vinflg = '1'
019600010618     C                   eval      flgOk = '1'
019700010618     C                   endif
019800040510     C*
019900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020000040510     C                   z-add     *zeros        wLenVINDTA        4 0
020100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020500040510     C                   z-add     1             wFlgVINDTA
020600040510     C                   leave
020700040510     C                   endif
020800040510     C                   enddo
020900090420     C*
021000090420     C                   add       1             rrnum
021100010618     C*
021200040510     C                   if        vindta > *blanks AND
021300090420     C                             wFlgVINDTA = 1
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
022500090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
022600090421     C                   if        %subst(%trim(vindta):1:1) = CharCSV
022700090421     C                   eval      vindta  = '*' + %trim(vindta)
022800090421     C                   endif
022900070925     C*
023000010330     C                   exsr      inzvar
023100010605     C                   exsr      impvab
023200010601     C*
023300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023400010605     C                   eval      depfil = VABLNP
023500010601     C                   exsr      repfil
023600010601     C                   if        depfil = invfil
023700021025     C                   if        vlrpoi = 999
023800030822     C                   move(P)   invfil        VABFGS
023900021025     C                   else
024000030822     C                   move(P)   vlrpoi        VABFGS
024100021025     C                   endif
024200010601     C*
024300010601     C                   exsr      PREELA
024400010601     C*
024500010604     C* Ebbene...
024600010604     C*
024700040119     C  N31              add       1             §CTROKVB
024800040119     C   32              add       1             §CTRMO
024900040119     C   31              add       1             §CTRNO
025000030822     C                   if        wGiro = 2
025100040510     C* Scrivo sempre il VAT
025200040510     C  N31              exsr      wrivat
025300040510     C* Scrivo il VAB solo a rottura d spedizione
025400040119     C                   if        depspe =  *zeros OR
025500040119     C                             depspe <> vabNSP
025600040119     C  N31              exsr      wrivab
025700040119     C  N31              eval      depspe = vabNSP
025800040119     C                   endif
025900040119     C                   endif
026000010604     C*
026100010604     C                   if        *in31 = *off and
026200010604     C                             *in32 = *off
026300010604     C                   eval      vinflg = '1'
026400010604     C                   else
026500010604     C                   eval      vinflg = '2'
026600010604     C                   endif
026700010604     C                   endif
026800010601     C*
026900010604     C                   endif
027000010604     C*
027100010330     C                   else
027200010330     C                   eval      vinflg = '1'
027300010330     C                   endif
027400010601     C*
027500010601     C  N70              update    tivin000
027600010330     C*
027700030822     C  N70              enddo
027800040119     C*
027900040119     C                   endif                                                  (endif - chkcall)
028000010601     C*
028100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028200010601     C                   if        cntNonEl = *zeros or
028300010601     C                             flgMulti = '0'
028400010330     C* Se non ci sono record con errori ...
028500010601     C                   if        §ctrno = 0 and
028600010604     C                             §ctrmo = 0 and
028700010601     C                             flgStato <> '2'
028800010330     C* ... restituisco esito OK.
028900010330     C                   eval      wrkesito = '0'
029000010330     C                   else
029100040119     C                   if        §ctrokvb > 0
029200010330     C                   eval      wrkesito = '1'
029300010330     C                   else
029400010615     C                   if        flgOk = '0'
029500010615     C                   eval      wrkesito = '2'
029600010615     C                   else
029700010615     C                   eval      wrkesito = '6'
029800010615     C                   endif
029900010330     C                   endif
030000010330     C                   endif
030100010601     C                   else
030200010601     C                   eval      wrkesito = '9'
030300010601     C                   endif
030400010330     C*
030500010330     C                   if        %open(tivin00r)
030600010330     C                   close     tivin00r
030700010330     C                   endif
030800021025     C                   if        %open(fivabwwr)
030900021025     C                   close     fivabwwr
031000010330     C                   endif
031100040512     C                   if        %open(fivatwwr)
031200040512     C                   close     fivatwwr
031300040512     C                   endif
031400010601     C*
031500010601     C                   if        vlrpoi <> 999
031600010601     C                   eval      invfil = vlrpoi
031700010601     C                   endif
031800010330     C*
031900040119     C                   if        §ctrokvb > 0
032000010601     C                             and invfil > *zeros
032100010330     C                   exsr      invio
032200010330     C                   endif
032300010330     C*
032400010330     C                   ENDSR
032500010330     C***
032600040119
032700040119
032800040119
032900040119     C*----------------------------------------------------*
033000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
033100040119     C*----------------------------------------------------*
033200040510     C     WRIVAB        BEGSR
033300040119     C*
033400060331     C* Gestisco l'eventuale rottura x numero spedizione
033500070823     C                   if        VABNSP <> depspe
033600070301     C                   if        wVATNOT_E <> *blanks
033700080125     C                   if        VABCTM = *blanks
033800080125     C                   eval      VABCTM = '7Q'
033900080125     C                   endif
034000070301     C                   endif
034100040119     C                   write     fivab000                                     => scarico il VAB
034200060331     C                   endif
034300040119     C*
034400040119     C                   ENDSR
034500040119
034600040119
034700040119
034800040119     C*----------------------------------------------------*
034900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
035000040119     C*----------------------------------------------------*
035100040510     C     WRIVAT        BEGSR
035200060512     C*
035300060512     C* Inizializzo i campi chiave
035400060512     C                   eval      VATFGS = VABFGS
035500060512     C                   eval      VATAAS = VABAAS
035600060512     C                   eval      VATCCM = VABCCM
035700070823     C                   eval      VATNRS = VABNRS
035800060512     C                   eval      VATNSP = VABNSP
035900060512     C                   eval      VATLNP = VABLNP
036000060331     C*
036100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
036200060331     C                   if        wVATNOT_A <> *blanks
036300091001     C                   eval      VATNOT = %trim(wVATNOT_A)
036400060331     C                   eval      VATTRC = 'A'
036500060331     C                   write     FIVAT000
036600060331     C                   add       1             §CTROKVT
036700060331     C                   endif
036800060331     C*
036900060331     C                   if        wVATNOT_B <> *blanks
037000091001     C                   eval      VATNOT = %trim(wVATNOT_B)
037100060331     C                   eval      VATTRC = 'B'
037200060331     C                   write     FIVAT000
037300060331     C                   add       1             §CTROKVT
037400060331     C                   endif
037500060331     C*
037600080625     C                   if        wVATNOT_E <> *blanks
037700091001     C                   eval      VATNOT = %trim(wVATNOT_E)
037800060331     C                   eval      VATTRC = 'E'
037900080716     C                   write     FIVAT000
038000080716     C                   add       1             §CTROKVT
038100080625     C                   endif
038200040119     C*
038300040119     C                   ENDSR
038400040119
038500040119
038600040119
038700040119     C*----------------------------------------------------*
038800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
038900040119     C*----------------------------------------------------*
039000040119     C     PREVAT        BEGSR
039100040119     C*
039200040119     C* Compongo il nome del membro da dare al FIVATWWR
039300040119     C                   eval      parmbr = vlrhdl
039400040119     C                   movel     'M'           parmbr
039500040510     C                   eval      parccm = vlrksc
039600040119     C                   eval      paropz = '1'
039700040119     C* Effettuo la chiamata al CLLE preposto
039800040119     C                   call(e)   'TITVVTC'
039900040119     C                   parm                    parccm
040000040119     C                   parm                    parmbr
040100040119     C                   parm                    paropz
040200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040300040119     C                   if        %error
040400040119     C                   movel     '1'           chkcall
040500040119     C                   else
040600040119     C                   movel     '0'           chkcall
040700040119     C                   endif
040800040119     C*
040900040119     C                   ENDSR
041000010601
041100010601
041200010601
041300010330     C*----------------------------------------------------*
041400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
041500010330     C*----------------------------------------------------*
041600040119     C     INZVAR        BEGSR
041700010330     C*
041800030822     C* Inizializzo variabili di wrk
041900020204     C                   Z-ADD     *zeros        Num5_0            5 0
042000100908     C                   CLEAR                   W_RSD
042100060331     C                   MOVEL     *blanks       wVATNOT_A
042200060331     C                   MOVEL     *blanks       wVATNOT_B
042300060331     C                   MOVEL     *blanks       wVATNOT_E
042400090220     C*
042500080617     C                   EVAL      wFlgCAS = *blanks
042600030822     C*
042700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
042800030822     C                   CLEAR                   FIVAB000
042900040119     C                   CLEAR                   FIVAT000
043000030822     C                   CLEAR                   SkSplitCSV
043100030822     C                   EVAL      i = 1
043200030822     C                   EVAL      posDa = *zeros
043300030822     C                   EVAL      posA  = *zeros
043400030822     C*
043500030822     C* Reimposto i valori di default
043600030822     C                   EXSR      DEFCAM
043700030822     C*
043800010330     C                   ENDSR
043900010330     C*----------------------------------------------------*
044000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
044100010330     C*----------------------------------------------------*
044200010330     C     DEFCAM        BEGSR
044300030822     C*
044400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
044500030715     C* e delimitatore testo.
044600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
044700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
044800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
044900030822     C*
045000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
045100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
045200030715     C                   SETON                                        50
045300030715     C                   ELSE
045400030715     C                   SETOFF                                       50
045500030715     C                   ENDIF
045600090421     C*
045700090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
045800090421     C                   EVAL      VABCBO = '1'
045900090421     C                   EVAL      VABLNP = vlrpoi
046000090421     C                   EVAL      VABNSP = rrnum
046100091026     C*
046200091026     C*** PERSONALIZZAZIONE CLIENTE ***
046300100908     C***                MOVEL     *blanks       wNOTE            70
046400100908     C***                EVAL      wNOTE = 'RICHIESTA VARIAZIONE INDIRIZZO ' +
046500100908     C***                          'E FERMO DEPOSITO AUTORIZZATO -NO GIACENZA'
046600100908     C***                EVAL      VABNOT = %subst(wNOTE:1:35)
046700100908     C***                EVAL      VABNT2 = %subst(wNOTE:36:35)
046800091026     C*********************************
046900030822     C*
047000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
047100030822     C                   EVAL      posDaDft = 1
047200030822     C                   EVAL      posADft  = 0
047300030822     C                   EVAL      wGiroDft = 0
047400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
047500030822     C                             posDaDft > 0
047600030822     C*
047700030822     C* Gestisco il 1° giro
047800030822     C                   IF        wGiroDft = 0
047900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
048000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
048100030822     C* Incremento il contatore dei "giri"
048200030822     C                   EVAL      wGiroDft = 1
048300030822     C                   ELSE
048400030822     C                   EVAL      posDaDft = posADft
048500030822     C                   ENDIF
048600030822     C* Eseguo lo scan x trovare la fine del campo corrente
048700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
048800030822     C*
048900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
049000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
049100030822     C                   IF        posDaDft > 0 AND
049200030822     C                             posADft  > 0
049300030822     C* NCL
049400030822     C                   IF        %subst(
049500030822     C                             %subst(vlrppt:posDaDft+1:
049600030822     C                             posADft-posDaDft-1):1:3)
049700030822     C                             = 'NCL'
049800030822     C                   EVAL      PiStr=%trim(%subst(
049900030822     C                             %subst(vlrppt:posDaDft+1:
050000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050100030822     C                   EXSR      CHKNUM
050200030822     C                   IF        PiInt=*on
050300030822     C                   Z-ADD     PiVal         VABNCL
050400030822     C                   ENDIF
050500030822     C                   ENDIF
050600030822     C* CCM
050700030822     C                   IF        %subst(
050800030822     C                             %subst(vlrppt:posDaDft+1:
050900030822     C                             posADft-posDaDft-1):1:3)
051000030822     C                             = 'CCM'
051100030822     C                   EVAL      PiStr=%trim(%subst(
051200030822     C                             %subst(vlrppt:posDaDft+1:
051300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051400030822     C                   EXSR      CHKNUM
051500030822     C                   IF        PiInt=*on
051600030822     C                   Z-ADD     PiVal         VABCCM
051700030822     C                   ENDIF
051800030822     C                   ENDIF
051900030822     C* LNP
052000030822     C                   IF        %subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):1:3)
052300030822     C                             = 'LNP'
052400030822     C                   EVAL      PiStr=%trim(%subst(
052500030822     C                             %subst(vlrppt:posDaDft+1:
052600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052700030822     C                   EXSR      CHKNUM
052800030822     C                   IF        PiInt=*on
052900030822     C                   Z-ADD     PiVal         VABLNP
053000030822     C                   ENDIF
053100030822     C                   ENDIF
053200030822     C* NRS
053300030822     C                   IF        %subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):1:3)
053600030822     C                             = 'NRS'
053700030822     C                   EVAL      PiStr=%trim(%subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054000030822     C                   EXSR      CHKNUM
054100030822     C                   IF        PiInt=*on
054200030822     C                   Z-ADD     PiVal         VABNRS
054300030822     C                   ENDIF
054400030822     C                   ENDIF
054500030822     C* CTR
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'CTR'
055000030822     C                   EVAL      PiStr=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   EXSR      CHKNUM
055400030822     C                   IF        PiInt=*on
055500030822     C                   Z-ADD     PiVal         VABCTR
055600030822     C                   ENDIF
055700030822     C                   ENDIF
055800030822     C* PKB
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'PKB'
056300030822     C                   EVAL      PiStr=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   EXSR      CHKNUM
056700030822     C                   IF        PiNum=*on
056800030822     C                   Z-ADD     PiVal         VABPKB
056900030822     C                   ENDIF
057000030822     C                   ENDIF
057100030822     C* VLB
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'VLB'
057600030822     C                   EVAL      PiStr=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   EXSR      CHKNUM
058000030822     C                   IF        PiNum=*on
058100030822     C                   Z-ADD     PiVal         VABVLB
058200030822     C                   ENDIF
058300030822     C                   ENDIF
058400030822     C* QFT
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'QFT'
058900030822     C                   EVAL      PiStr=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   EXSR      CHKNUM
059300030822     C                   IF        PiNum=*on
059400030822     C                   Z-ADD     PiVal         VABQFT
059500030822     C                   ENDIF
059600030822     C                   ENDIF
059700030822     C* CBO
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'CBO'
060200030822     C                   EVAL      VABCBO=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   ENDIF
060600030822     C* TSP
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'TSP'
061100030822     C                   EVAL      VABTSP=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   ENDIF
061500030822     C* VAS
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'VAS'
062000030822     C                   EVAL      VABVAS=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   ENDIF
062400030822     C* VCA
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'VCA'
062900030822     C                   EVAL      VABVCA=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   ENDIF
063300070925     C* TIC
063400070925     C                   IF        %subst(
063500070925     C                             %subst(vlrppt:posDaDft+1:
063600070925     C                             posADft-posDaDft-1):1:3)
063700070925     C                             = 'TIC'
063800070925     C                   EVAL      VABTIC=%trim(%subst(
063900070925     C                             %subst(vlrppt:posDaDft+1:
064000070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100070925     C                   ENDIF
064200030822     C* GCA
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'GCA'
064700030822     C                   EVAL      VABGCA=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   ENDIF
065100030822     C* CTM
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'CTM'
065600030822     C                   EVAL      VABCTM=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* FFD
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'FFD'
066500030822     C                   EVAL      VABFFD=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* VAD
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'VAD'
067400030822     C                   EVAL      VABVAD=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* GMA
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'GMA'
068300030822     C                   EVAL      VABGMA=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* GGA
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'GGA'
069200030822     C                   EVAL      VABGGA=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* GVA
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'GVA'
070100030822     C                   EVAL      VABGVA=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* TC1
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'TC1'
071000030822     C                   EVAL      VABTC1=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* TC2
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'TC2'
071900030822     C                   EVAL      VABTC2=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300040714     C* VATTRC
072400040714     C                   IF        %subst(
072500040714     C                             %subst(vlrppt:posDaDft+1:
072600040714     C                             posADft-posDaDft-1):1:3)
072700040714     C                             = 'TRC'
072800040714     C                   EVAL      VATTRC=%trim(%subst(
072900040714     C                             %subst(vlrppt:posDaDft+1:
073000040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100040714     C                   ENDIF
073200030822     C* ...
073300030822     C                   ENDIF
073400030822     C                   ENDDO
073500020204     C*
073600010330     C                   ENDSR
073700010607     C*----------------------------------------------------*
073800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
073900010607     C*----------------------------------------------------*
074000010607     C     IMPVAB        BEGSR
074100010607     C*
074200010607     C                   SETOFF                                       3132
074300030515     C*
074400010607     C                   MOVEL     datcor        VABAAS
074500010607     C                   MOVE      datcor        VABMGS
074600010607     C*
074700030715     C**********
074800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
074900030715     C**********
075000030715     C                   IF        wGiro = *zeros
075100080716     C*
075200080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
075300080716     C                   EVAL      depvinDTA = vinDTA
075400040113     C*
075500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
075600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
075700040510     C                             %trim(intcol(3))<>*blanks
075800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
075900040113     C                                      %trim(intcol(3))
076000070823     C     ';':CharCSV   XLATE     vindta        vindta
076100040510     C                   ENDIF
076200080626     C*
076300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
076400031201     C     minu:maiu     XLATE     vindta        vindta
076500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
076600030715     C                   DOW       posDa <= %len(%trim(vindta))
076700030715     C*
076800030715     C* Gestisco il 1° campo
076900030715     C                   IF        i = 1
077000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
077100030715     C                   EVAL      posDa = 1
077200030715     C* Eseguo lo scan x trovare la fine del primo campo
077300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077400030715     C* A questo punto "estraggo" il campo corrente
077500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077600030715     C                                                    (posA-posDa))
077700030715     C* X i campi successivi al 1°
077800030715     C                   ELSE
077900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078000030715     C                   EVAL      posDa = posA + 1
078100030715     C* Eseguo lo scan x trovare la fine del campo corrente
078200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078300030715     C* Gestisco l'ultimo campo
078400030715     C                   IF        posA = *zeros
078500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
078600030715     C* A questo punto "estraggo" il campo corrente
078700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078800030715     C                                                    (posA-posDa))
078900030715     C                   LEAVE
079000030715     C                   ELSE
079100030715     C* A questo punto "estraggo" il campo corrente
079200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079300030715     C                                                    (posA-posDa))
079400030715     C                   ENDIF
079500030715     C                   ENDIF
079600030715     C* Incremento il contatore d campo
079700030715     C                   EVAL      i = i +1
079800030715     C                   ENDDO
079900030715     C*
080000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
080100030715     C* delimitazione testo
080200030715     C                   EVAL      i = 1
080300030715     C                   DOW       i < %elem(SkSplitFLD)
080400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
080500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
080600030715     C                   EVAL      i = i + 1
080700030715     C                   ENDDO
080800030715     C                   EVAL      wGiro = 1
080900080716     C* Ripristino la 1° riga originale
081000080716     C                   EVAL      vinDTA = depvinDTA
081100030715     C                   ELSE
081200030715     C                   EVAL      wGiro = 2
081300031204     C**********
081400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
081500031204     C* d separatore campo
081600031204     C**********
081700031204     C                   EVAL      vindta = %trim(vindta)
081800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
081900031204     C                   EVAL      vindta = %subst(vindta:2)
082000031204     C                   ENDDO
082100031204     C*
082200031204     C                   Z-ADD     *zeros        lunghInput        4 0
082300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
082400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
082500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
082600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
082700031204     C                   ENDDO
082800030715     C**********
082900030715     C* Effettuo lo split del campo dati d input
083000030715     C**********
083100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083200030715     C                   DOW       posDa <= %len(%trim(vindta))
083300030715     C*
083400030715     C* Gestisco il 1° campo
083500030715     C                   IF        i = 1
083600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083700030715     C                   EVAL      posDa = 1
083800030715     C* Eseguo lo scan x trovare la fine del primo campo
083900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084000030715     C* A questo punto "estraggo" il campo corrente
084100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084200030715     C                                                    (posA-posDa))
084300030715     C* X i campi successivi al 1°
084400030715     C                   ELSE
084500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084600030715     C                   EVAL      posDa = posA + 1
084700031204     C* Verifico che nn vi sia il campo nn valorizzato
084800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
084900031204     C* Se campo nn valorizzato skippo al prossimo
085000031204     C                   EVAL      SkSplitCSV(i) = *blanks
085100031204     C                   EVAL      posA  = posA + 1
085200031204     C                   ELSE
085300030715     C* Eseguo lo scan x trovare la fine del campo corrente
085400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085500030715     C* Gestisco l'ultimo campo
085600030715     C                   IF        posA = *zeros
085700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085800030715     C* A questo punto "estraggo" il campo corrente
085900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086000030715     C                                                    (posA-posDa))
086100030715     C                   LEAVE
086200030715     C                   ELSE
086300030715     C* A questo punto "estraggo" il campo corrente
086400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086500030715     C                                                    (posA-posDa))
086600030715     C                   ENDIF
086700031204     C                   ENDIF
086800030715     C                   ENDIF
086900030715     C* Incremento il contatore d campo
087000030715     C                   EVAL      i = i +1
087100030715     C                   ENDDO
087200030715     C*
087300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087400030715     C* delimitazione testo
087500030715     C                   EVAL      i = 1
087600030715     C                   DOW       i < %elem(SkSplitCSV)
087700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
087800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
087900030715     C                   EVAL      i = i + 1
088000030715     C                   ENDDO
088100030715     C*
088200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
088300030715     C                   EVAL      i = 1
088400030715     C                   DOW       i < %elem(SkSplitFLD)
088500030715     C*
088600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
088700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
088800030715     C                             SkSplitCSV(i) <> *blanks
088900030721     C*** ATB
089000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
089100030721     C                   EVAL      VABATB = SkSplitCSV(i)
089200030721     C                   ENDIF
089300030715     C*** RSD
089400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
089500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
089600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
089700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
089800030715     C* ==
089900030715     C                   ENDIF
090000030721     C*** RD2
090100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
090200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
090300030721     C                   ENDIF
090400030715     C*** IND
090500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
090600030715     C                   EVAL      VABIND = SkSplitCSV(i)
090700030715     C                   ENDIF
090800030715     C*** LOD
090900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
091000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
091100030715     C                   ENDIF
091200030715     C*** PRD
091300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
091400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
091500030715     C                   ENDIF
091600030721     C*** NZD
091700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
091800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
091900030721     C                   ENDIF
092000030715     C*** RMA
092100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
092200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
092300030715     C                   ENDIF
092400030715     C*** NOT
092500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
092600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
092700030715     C                   ENDIF
092800030715     C*** NT2
092900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
093000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
093100030715     C                   ENDIF
093200030715     C*** VCA
093300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
093400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
093500030715     C                   ENDIF
093600030715     C*** VAS
093700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
093800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
093900030715     C                   ENDIF
094000030715     C*** FFD
094100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
094200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
094300030715     C                   ENDIF
094400030721     C*** GC1
094500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
094600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
094700030721     C                   ENDIF
094800030721     C*** GC2
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
095000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030721     C*** TSP
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
095400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
095500030721     C                   ENDIF
095600040510     C*** CBO
095700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
095800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
095900040510     C                   ENDIF
096000030721     C*** NAS
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
096200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** TIC
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
096600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** GCA
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
097000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** XCO
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
097400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** CTM
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
097800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** TCR
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
098200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** CTS
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
098600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** FTM
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
099000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** VAD
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
099400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** GMA
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
099800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** GGA
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
100200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** GVA
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
100600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** TC1
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
101000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** TC2
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
101400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** SCL
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
101800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** RMO
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
102200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** NMO
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
102600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*
102900030721     C* Reperisco quindi i campi numerici...
103000040510     C*** NSP
103100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
103200040510     C                   EVAL      PiStr=SkSplitCSV(i)
103300040510     C                   EXSR      CHKNUM
103400040510     C                   IF        PiInt=*on
103500040510     C                   Z-ADD     PiVal         VABNSP
103600040510     C                   ELSE
103700040510     C                   SETON                                        31
103800040510     C                   EVAL      VABNSP = *zeros
103900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
104000040510     C                             + ' ' + 'VABNSP'
104100040510     C                   ENDIF
104200040510     C                   ENDIF
104300071106     C*** RMN
104400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
104500071106     C                   EVAL      PiStr=SkSplitCSV(i)
104600071106     C                   EXSR      CHKNUM
104700071106     C                   IF        PiInt=*on
104800071106     C                   Z-ADD     PiVal         VABRMN
104900071106     C                   ELSE
105000071106     C                   SETON                                        32
105100071106     C                   EVAL      VABRMN = *zeros
105200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
105300071106     C                             + ' ' + 'VABRMN'
105400071106     C                   ENDIF
105500071106     C                   ENDIF
105600030721     C*** CAD
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
105800030721     C                   EVAL      PiStr=SkSplitCSV(i)
105900030721     C                   EXSR      CHKNUM
106000030721     C                   IF        PiInt=*on
106100030721     C                   Z-ADD     PiVal         Num5_0
106200030721     C                   MOVEL(p)  Num5_0        VABCAD
106300030721     C                   ELSE
106400090603     C                   EVAL      VABCAD = SkSplitCSV(i)
106500090603     C                   SETON                                        32
106600090603     C                   EVAL      VABCAD = *zeros
106700090603     C                   EVAL      vinmsg = %trimr(vinmsg)
106800090603     C                             + ' ' + 'VABCAD'
106900030721     C                   ENDIF
107000030721     C                   ENDIF
107100030721     C*** NCL
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
107300030721     C                   EVAL      PiStr=SkSplitCSV(i)
107400030721     C                   EXSR      CHKNUM
107500030721     C                   IF        PiInt=*on
107600030721     C                   Z-ADD     PiVal         VABNCL
107700030721     C                   ELSE
107800030721     C                   SETON                                        32
107900030721     C                   EVAL      VABNCL = *zeros
108000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108100040122     C                             + ' ' + 'VABNCL'
108200030721     C                   ENDIF
108300030721     C                   ENDIF
108400030721     C*** PKB
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
108600030721     C                   EVAL      PiStr=SkSplitCSV(i)
108700030721     C                   EXSR      CHKNUM
108800030721     C                   IF        PiNum=*on
108900030721     C                   Z-ADD     PiVal         VABPKB
109000030721     C                   ELSE
109100030721     C                   SETON                                        32
109200030721     C                   EVAL      VABPKB = *zeros
109300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109400030721     C                             + ' ' + 'VABPKB'
109500030721     C                   ENDIF
109600030721     C                   ENDIF
109700030721     C*** CAS
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
109900080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
110000080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
110100081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
110200081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
110300080125     C                   EVAL      wFlgCAS = '1'
110400080125     C                   ENDIF
110500030721     C                   EVAL      PiStr=SkSplitCSV(i)
110600030721     C                   EXSR      CHKNUM
110700030721     C                   IF        PiNum=*on
110800030721     C                   Z-ADD     PiVal         VABCAS
110900030721     C                   ELSE
111000030721     C                   SETON                                        32
111100030721     C                   EVAL      VABCAS = *zeros
111200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111300030721     C                             + ' ' + 'VABCAS'
111400030721     C                   ENDIF
111500030721     C                   ENDIF
111600030721     C*** CCM
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
111800030721     C                   EVAL      PiStr=SkSplitCSV(i)
111900030721     C                   EXSR      CHKNUM
112000030721     C                   IF        PiInt=*on      AND
112100030721     C                             PiVal<=9999999 AND
112200030721     C                             PiVal>*zeros
112300030721     C                   Z-ADD     PiVal         VABCCM
112400030721     C                   ELSE
112500030721     C                   SETON                                        32
112600030721     C                   EVAL      VABCCM = *zeros
112700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112800040510     C                             + ' ' + 'VABCCM'
112900030721     C                   ENDIF
113000030721     C                   ENDIF
113100030721     C*** LNP
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
113300030721     C                   EVAL      PiStr=SkSplitCSV(i)
113400030721     C                   EXSR      CHKNUM
113500030721     C                   IF        PiInt=*on    AND
113600030721     C                             PiVal<=999   AND
113700030721     C                             PiVal>*zeros
113800030721     C                   Z-ADD     PiVal         VABLNP
113900030721     C                   ELSE
114000040715     C                   SETON                                        32
114100030721     C                   EVAL      VABLNP = *zeros
114200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114300040510     C                             + ' ' + 'VABLNP'
114400030721     C                   ENDIF
114500030721     C                   ENDIF
114600030721     C*** NRS
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiInt=*on AND
115100030721     C                             PiVal<=99
115200030721     C                   Z-ADD     PiVal         VABNRS
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600030721     C                             + ' ' + 'VABNRS'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900030721     C*** LNA
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiInt=*on    AND
116400030721     C                             PiVal<=999   AND
116500030721     C                             PiVal>*zeros
116600030721     C                   Z-ADD     PiVal         VABLNA
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      VABLNA = *zeros
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100030721     C                             + ' ' + 'VABLNA'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** CTR
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on  AND
117900030721     C                             PiVal<=999
118000030721     C                   Z-ADD     PiVal         VABCTR
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABCTR'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** IAS
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiNum=*on
119200030721     C                   Z-ADD     PiVal         VABIAS
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABIAS'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** VLB
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiNum=*on
120400030721     C                   Z-ADD     PiVal         VABVLB
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800030721     C                             + ' ' + 'VABVLB'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** QFT
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiNum=*on
121600030721     C                   Z-ADD     PiVal         VABQFT
121700030721     C                   ELSE
121800030721     C                   SETON                                        32
121900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122000030721     C                             + ' ' + 'VABQFT'
122100030721     C                   ENDIF
122200030721     C                   ENDIF
122300030721     C*** NCD
122400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiInt=*on      AND
122800030721     C                             PiVal<=9999999
122900030721     C                   Z-ADD     PiVal         VABNCD
123000030721     C                   ELSE
123100030721     C                   SETON                                        32
123200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123300030721     C                             + ' ' + 'VABNCD'
123400030721     C                   ENDIF
123500030721     C                   ENDIF
123600030721     C*** NCA
123700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
123800030721     C                   EVAL      PiStr=SkSplitCSV(i)
123900030721     C                   EXSR      CHKNUM
124000030721     C                   IF        PiInt=*on      AND
124100030721     C                             PiVal<=9999999
124200030721     C                   Z-ADD     PiVal         VABNCA
124300030721     C                   ELSE
124400030721     C                   SETON                                        32
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600030721     C                             + ' ' + 'VABNCA'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030721     C*** ZNC
125000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
125100030721     C                   EVAL      PiStr=SkSplitCSV(i)
125200030721     C                   EXSR      CHKNUM
125300030721     C                   IF        PiInt=*on AND
125400030721     C                             PiVal<=99
125500030721     C                   Z-ADD     PiVal         VABZNC
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900030721     C                             + ' ' + 'VABZNC'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** DCR
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiInt=*on       AND
126700030721     C                             PiVal<=99999999
126800030721     C                   Z-ADD     PiVal         VABDCR
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABDCR'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500030721     C*** HCR
127600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
127700030721     C                   EVAL      PiStr=SkSplitCSV(i)
127800030721     C                   EXSR      CHKNUM
127900030721     C                   IF        PiInt=*on   AND
128000030721     C                             PiVal<=9999
128100030721     C                   Z-ADD     PiVal         VABHCR
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABHCR'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** VMD
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiNum=*on
129300030721     C                   Z-ADD     PiVal         VABVMD
129400030721     C                   ELSE
129500030721     C                   SETON                                        32
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABVMD'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000030721     C*** ANT
130100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
130200030721     C                   EVAL      PiStr=SkSplitCSV(i)
130300030721     C                   EXSR      CHKNUM
130400030721     C                   IF        PiInt=*on        AND
130500030721     C                             PiVal<=999999999
130600030721     C                   Z-ADD     PiVal         VABANT
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABANT'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** CMO
131400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
131500030721     C                   EVAL      PiStr=SkSplitCSV(i)
131600030721     C                   EXSR      CHKNUM
131700030721     C                   IF        PiInt=*on
131800030721     C                   Z-ADD     PiVal         Num5_0
131900030721     C                   MOVEL(p)  Num5_0        VABCMO
132000030721     C                   ELSE
132100030721     C                   SETON                                        32
132200030721     C                   EVAL      VABCMO = *zeros
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABCMO'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700040510     C*
132800040510     C* Reperisco i campi del FIVAT
132900040510     C*** VATNOT
133000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
133100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
133200040510     C                   ENDIF
133300040510     C*** VATTRC
133400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
133500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
133600040510     C                   ENDIF
133700060331     C*** VATNOT_A
133800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
133900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
134000060331     C                   ENDIF
134100060331     C*** VATNOT_B
134200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
134300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
134400060331     C                   ENDIF
134500060331     C*** VATNOT_E
134600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
134700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
134800060331     C                   ENDIF
134900070823     C*
135000071029     C***  ===>  Gestione campi "particolari" dentro ciclo
135100090421     C*
135200090921     C*** W_VABRSD
135300090622     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
135400100908     C                   MOVEL     SkSplitCSV(i) W_RSD
135500090622     C                   ENDIF
135600100112     C*** W_VATNOTA1
135700100112     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTA1'
135800100112     C                   EVAL      wVATNOT_A = %trim(wVATNOT_A) + ' ' +
135900090921     C                                         %trim(SkSplitCSV(i))
136000090921     C                   ENDIF
136100100112     C*** W_VATNOTB1
136200100112     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTB1'
136300090921     C                   EVAL      wVATNOT_B = %trim(wVATNOT_B) + ' ' +
136400090921     C                                         %trim(SkSplitCSV(i))
136500090921     C                   ENDIF
136600070925     C*
136700071029     C***  <===  -----------------------------------------
136800040510     C*
136900030715     C                   ENDIF
137000030715     C                   EVAL      i = i + 1
137100030715     C                   ENDDO
137200070925     C*
137300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
137400090921     C*
137500090921     C*
137600090921     C* NSP => Stacco un numeratore da AZNUM
137700090921     C                   clear                   TRUL33DS
137800090921     C                   eval      I33OPE = *zeros
137900090921     C                   eval      I33CNU = 302
138000090921     C                   eval      I33NUM = 1
138100090921     C                   movel     TRUL33DS      KPJBU
138200090921     C                   call      'TRUL33R'
138300090921     C                   parm                    KPJBA
138400090921     C                   movel     KPJBU         TRUL33DS
138500090921     C                   if        O33ERR = *zeros
138600090921     C                   z-add     O33NRF        VABNSP
138700090921     C                   z-add     O33NRF        VATNSP
138800090921     C                   else
138900090921     C                   SETON                                        31
139000090921     C                   EVAL      vinmsg = %trimr(vinmsg)
139100090921     C                             + ' ' + 'VABNSP VATNSP'
139200090921     C                   endif
139300080125     C*
139400100908     C                   EVAL      VABRSD = W_RSD_1
139500100908     C                   EVAL      VABRD2 = W_RSD_2
139600100908     C                   EVAL      VABNOT = W_RSD_3
139700100908     C                   EVAL      VABNT2 = W_RSD_4
139800070925     C*
139900071029     C***  <===  -----------------------------------------
140000101220     C*
140100101220     C* Ulteriore eventuale forzatura successiva
140200101220     C                   IF        VABNOT+VABNT2 = *blanks
140300101220     C                   EVAL      VABNOT =
140400101220     C                             'VARIAZIONE INDIR. E FERMO DEP. '
140500101220     C                   EVAL      VABNT2 =
140600101220     C                             'AUTORIZZATO - NO GIACENZA'
140700101220     C                   ENDIF
140800070925     C*
140900070925     C* Considerazioni finali su CBO/CAS
141000080125     C                   IF        wFlgCAS = '1'
141100070925     C                   IF        VABCBO = '1'
141200070925     C                   EVAL      VABCBO = '4'
141300070925     C                   ENDIF
141400070925     C                   IF        VABCBO = '2'
141500070925     C                   EVAL      VABCBO = '6'
141600070925     C                   ENDIF
141700070925     C                   ENDIF
141800070925     C***  <===  ----------------------------
141900040510     C*
142000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
142100040510     C                   IF        VABNSP = *zeros
142200040510     C                   SETON                                        31
142300040510     C                   ENDIF
142400040715     C*
142500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
142600040715     C                   IF        VABLNP = *zeros
142700040715     C                   SETON                                        31
142800040715     C                   ENDIF
142900040510     C*
143000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
143100040510     C                   IF        VABRMN = *zeros
143200040510     C                   Z-ADD     VABNSP        VABRMN
143300040510     C                   ENDIF
143400040510     C*
143500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
143600040510     C                   IF        VABRMA = *blanks
143700040714     C                   MOVEL     VABRMN        VABRMA
143800040510     C                   ENDIF
143900040128     C*
144000040510     C* Se provincia nn valorizzata la reperisco
144100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
144200040510     C                   IF        VABPRD = *blanks
144300040128     C                   CLEAR                   TISI95DS
144400040128     C                   EVAL      I95TCN = '3'
144500040128     C                   Z-ADD     datcor        I95DAT
144600040128     C                   EVAL      I95NAR = VABNZD
144700040128     C                   EVAL      I95CAP = VABCAD
144800040128     C                   EVAL      I95LOC = VABLOD
144900040128     C                   CALL      'TISI95R'
145000040128     C                   PARM                    TISI95DS
145100040128     C                   EVAL      VABPRD = O95PRV
145200040510     C                   ENDIF
145300020204     C*
145400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
145500020204     C                   EXSR      CHKIMPDIV
145600030715     C                   ENDIF
145700020204     C*
145800010607     C                   ENDSR
145900010607     C*----------------------------------------------------*
146000010601
146100020204
146200020204     C*----------------------------------------------------*
146300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
146400020204     C*----------------------------------------------------*
146500020204     C     CHKIMPDIV     BEGSR
146600020204     C*
146700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
146800020204     C                   Z-ADD     *zeros        wrkDec            9 9
146900020204     C*
147000020204     C* Come prima cosa effettuo considerazioni sulla divisa
147100020204     C                   IF        vabIAS > *zeros
147200020204     C                   IF        vabVAS <> 'EUR'
147300020204     C                   EVAL      vabVAS =  'ITL'
147400020204     C                   ENDIF
147500020204     C                   ENDIF
147600020204     C*
147700020204     C                   IF        vabCAS > *zeros
147800020204     C                   IF        vabVCA <> 'EUR'
147900020204     C                   EVAL      vabVCA =  'ITL'
148000020204     C                   ENDIF
148100020204     C                   ENDIF
148200020204     C*
148300020204     C                   IF        vabVMD > *zeros
148400020204     C                   IF        vabVAD <> 'EUR'
148500020204     C                   EVAL      vabVAD =  'ITL'
148600020204     C                   ENDIF
148700020204     C                   ENDIF
148800020204     C*
148900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
149000020204     C                   Z-ADD     vabIAS        wrkDec
149100020204     C                   IF        wrkDec > *zeros
149200020204     C                   IF        vabVAS = 'ITL'
149300020204     C                   EVAL      vabIAS = *zeros
149400020204     C                   ENDIF
149500020204     C                   ENDIF
149600020204     C*
149700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
149800020204     C                   Z-ADD     vabCAS        wrkDec
149900020204     C                   IF        wrkDec > *zeros
150000020204     C                   IF        vabVCA = 'ITL'
150100020204     C                   EVAL      vabCAS = *zeros
150200020204     C                   ENDIF
150300020204     C                   ENDIF
150400020204     C*
150500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
150600020204     C                   Z-ADD     vabVMD        wrkDec
150700020204     C                   IF        wrkDec > *zeros
150800020204     C                   IF        vabVAD = 'ITL'
150900020204     C                   EVAL      vabVMD = *zeros
151000020204     C                   ENDIF
151100020204     C                   ENDIF
151200020204     C*
151300020204     C                   ENDSR
151400020204     C***
151500080617
151600080617
151700080617
151800080617     C*----------------------------------------------------*
151900080617     C*  CONTROLLO NUMERICITA' CAMPI
152000080617     C*----------------------------------------------------*
152100080617     C     CHKNUM        BEGSR
152200080617     C*
152300080617     C                   IF        PiDecChr = *blanks
152400080617     C                   EVAL      PiDecChr = CharNUM
152500080617     C                   ENDIF
152600080617     C*
152700080617     C                   callp     UBISNUM_Check(PiStr
152800080617     C                                          :PiDecChr
152900080617     C                                          :PiVal
153000080617     C                                          :PiNum
153100080617     C                                          :PiInt)
153200080617     C*
153300080617     C                   ENDSR
153400080617     C***
153500010330
153600010601
153700010601
153800010601
153900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
154000010601     C     repfil        BEGSR
154100010601     C*
154200010601     C                   if        invfil = *zeros and
154300010601     C                             depfil > *zeros and
154400010629     C                             (vinflg = *blanks or
154500010629     C                              vinflg = *zeros)
154600010601     C
154700010601     C                   eval      invfil = depfil
154800010601     C                   endif
154900010601     C*
155000010601     C                   if        depfil <> invfil and
155100010601     C                             invfil > *zeros
155200010601     C                   eval      flgMulti = '1'
155300010601     C                   if        vinflg = *blanks
155400010601     C                   add       1             cntNonEl
155500010601     C                   endif
155600010601     C                   endif
155700010601     C*
155800010601     C                   if        vinflg = '2'
155900010601     C                   eval      flgStato = '2'
156000010601     C                   endif
156100010601     C*
156200010601     C                   ENDSR
156300010601     C***
156400010601
156500010601
156600010601
156700010330
156800040119      /TITLE Invio dei dati al punto operativo.
156900040119     C     invio         BEGSR
157000040119     C*
157100040119     C* 1° invio FIVAT
157200040119     C                   reset                   dscmz
157300040119     C                   move      vlrpoi        cmzdst
157400040119     C                   eval      cmzfld = 'FIVATWWR'
157500040119     C                   eval      cmzmbd = vlrhdl
157600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
157700040119     C***                if        prmfir = *blanks
157800040119     C                   eval      cmzfla = 'FIVAT00F'
157900040119     C                   eval      cmzmba = 'FIVAT00F'
158000040119     C***                else
158100040119     C***                eval      cmzfla = prmfir
158200040119     C***                eval      cmzmba = prmfir
158300040119     C***                endif
158400040119     C                   eval      cmznrr = *zeros
158500040119     C                   move      §ctrokvt      cmznrr
158600040119     C                   eval      cmzlba = vlrfl1
158700040119     C                   call(e)   'TIS711C'
158800040119     C                   parm                    dscmz
158900040119     C                   parm      *blanks       esito
159000040119     C                   if        %error
159100040119     C                             or cmzerr = '1'
159200040119     C                             or esito  = '1'
159300040119     C                   eval      wrkesito = '3'
159400040119     C                   else
159500040119     C*
159600040119     C* 2° invio FIVAB
159700040119     C                   reset                   dscmz
159800040119     C                   move      vlrpoi        cmzdst
159900040119     C                   eval      cmzfld = vlrfou
160000040119     C                   eval      cmzmbd = vlrhdl
160100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
160200040119     C***                if        prmfir = *blanks
160300040119     C                   eval      cmzfla = 'FIVAB00F'
160400040119     C                   eval      cmzmba = 'FIVAB00F'
160500040119     C***                else
160600040119     C***                eval      cmzfla = prmfir
160700040119     C***                eval      cmzmba = prmfir
160800040119     C***                endif
160900040119     C                   eval      cmznrr = *zeros
161000040119     C                   move      §ctrokvb      cmznrr
161100040119     C                   eval      cmzlba = vlrfl1
161200040119     C                   call(e)   'TIS711C'
161300040119     C                   parm                    dscmz
161400040119     C                   parm      *blanks       esito
161500040119     C                   if        %error
161600040119     C                             or cmzerr = '1'
161700040119     C                             or esito  = '1'
161800040119     C                   eval      wrkesito = '3'
161900040119     C                   endif
162000040119     C                   endif
162100040119     C*
162200040119     C                   ENDSR
162300040119     C***
162400010601
162500010601
162600010601
162700010601
162800010601      /TITLE Invio dei dati al punto operativo.
162900010601     C     opeini        BEGSR
163000010601     C*
163100010601     C* Inizializzo flag e contatori operativi
163200010601     C                   movel     '0'           flgGiro           1
163300010601     C                   movel     '0'           flgMulti          1
163400010601     C                   movel     '1'           flgStato          1
163500010615     C                   movel     '0'           flgOk             1
163600010601     C                   z-add     *zeros        cntNonEl         10 0
163700010601     C                   z-add     *zeros        depfil            3 0
163800010601     C                   z-add     *zeros        invfil            3 0
163900010601     C*
164000010601     C                   ENDSR
164100010601     C***
164200070326
164300070326
164400070326
164500070326
164600080916     C     *pssr         BEGSR
164700070329     C*
164800080916     C                   if        %open(tivin00r)
164900080916     C                   close     tivin00r
165000080916     C                   endif
165100080916     C                   if        %open(fivabwwr)
165200080916     C                   close     fivabwwr
165300080916     C                   endif
165400080916     C                   if        %open(fivatwwr)
165500080916     C                   close     fivatwwr
165600080916     C                   endif
165700070326     C*
165800070326     C* Effettuo la chiamata al CLLE preposto
165900080916     C                   call(e)   'TITVVTC'
166000080916     C                   parm                    parccm
166100080916     C                   parm                    parmbr
166200080916     C                   parm      '2'           paropz
166300070326     C*
166400080916     C                   eval      wrkesito = '2'
166500070404     C*
166600080916     C                   seton                                        LR
166700070326     C*
166800080916     C                   ENDSR     '*CANCL'
166900070326     C***
167000070326
167100070326
167200010330
167300010330
167400000613     C     *inzsr        BEGSR
167500990910     C*
167600990910     C     *entry        plist
167700990920     C                   parm                    tivlrds
167800990921     C                   parm      wrkesito      esito
167900000724     C                   parm                    prmlit
168000000710     C                   parm                    prmfir
168100010330     C*
168200010330     C* CALCOLA LA DATA CORRENTE
168300010330     C                   time                    wn14             14 0
168400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
168500010330     C                   z-add     wn8           g08dat
168600010330     C                   z-add     *zeros        g08inv
168700010330     C                   movel     '0'           g08err
168800010330     C                   call      'XSRDA8'
168900010330     C                   parm                    wlbda8
169000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
169100000613     C*
169200000613     C                   ENDSR
169300000613     C***
169400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
169500090921VABRMN;VABRMA;FIL001;W_VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNAS;
169600100112FIL002;W_VATNOTA1;W_VATNOTB1;VATNOT_E;
169700090921
