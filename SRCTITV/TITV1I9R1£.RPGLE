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
004700120330     D wVATNOT_I       s                   LIKE(VATNOT)
004800120330     D wVATNOT_J       s                   LIKE(VATNOT)
004900030822     D*------------------
005000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005100030822     D*------------------
005200030822     D posDaDft        S              3  0 INZ(*zeros)
005300030822     D posADft         S              3  0 INZ(*zeros)
005400030822     D j               s              3  0 INZ(1)
005500030822     D wGiroDft        s              1  0 INZ(*zeros)
005600010330     D*------------------
005700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005800010330     D*------------------
005900010330     D WLBDA8          DS                  INZ
006000010330     D  G08DAT                 1      8  0
006100010330     D  G08INV                 9     16  0
006200010330     D  G08ERR                17     17
006300010330     D  G08TGI                18     22  0
006400031201     D*------------------
006500031201     D* Costanti
006600031201     D*------------------
006700031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006800031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006900080617     D*------------------
007000080617     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
007100080617     D*------------------
007200080617     D PiStr           S            100A   INZ
007300080617     D PiDecChr        S              1A   INZ
007400080617     D PiVal           S             63S30 INZ
007500080617     D PiInt           S               N   INZ
007600080617     D PiNum           S               N   INZ
007700080617
007800080617     D*------------------
007900080617     D* LINKING A DEFINIZIONI ESTERNE
008000080617     D*------------------
008100080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008200010330
008300010330
008400010330
008500000913     C                   reset                   rrnum
008600990921     C                   reset                   esito
008700990921     C                   reset                   wrkesito
008800010601     C*
008900010601     C                   exsr      opeini
009000010605     C                   exsr      rwvab
009100010601     C*
009200010601     C                   seton                                        lr
009300010601
009400010601
009500010601
009600010601     C*--------------------------------------------------------
009700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009800010601     C*--------------------------------------------------------
009900010601     C     PREELA        BEGSR
010000010601     C*
010100010601     C* SE OCCORRE SPEDIRE IN FILIALE
010200010601     C                   if        invfil <> *zeros and
010300010601     C                             flgGiro = '0'
010400010601     C*
010500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010600010601     C                   eval      flgGiro = '1'
010700010601     C*
010800010601     C                   endif
010900010601     C*
011000010601     C                   ENDSR
011100010601     C***
011200010601
011300010601
011400010601
011500010601
011600010601     C*--------------------------------------------------------
011700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011800010601     C*--------------------------------------------------------
011900010601     C     ENDELA        BEGSR
012000040119     C*
012100040119     C* Effettuo la chiamata al CLLE preposto
012200040119     C                   call(e)   'TITVVTC'
012300040510     C                   parm                    parccm
012400040119     C                   parm                    parmbr
012500040119     C                   parm      '2'           paropz
012600050201     C*
012700050201     C* Effettuo lancio TISI95 solo x chiusura
012800050201     C                   CLEAR                   TISI95DS
012900050201     C                   EVAL      I95TLA = 'C'
013000050201     C                   CALL      'TISI95R'
013100050201     C                   PARM                    TISI95DS
013200000616     C*
013300010601     C                   ENDSR
013400010601     C***
013500000613
013600010601
013700010601
013800010330     C*--------------------------------------------------------
013900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014000010330     C*--------------------------------------------------------
014100010605     C     RWVAB         BEGSR
014200010330     C*
014300010330     C                   if        not %open(tivin00r)
014400010330     C                   open      tivin00r
014500010330     C                   endif
014600021025     C                   if        not %open(fivabwwr)
014700021025     C                   open      fivabwwr
014800010330     C                   endif
014900040119     C*
015000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015100040119     C                   exsr      prevat
015200040119     C*
015300040119     C                   if        chkcall = '0'
015400040119     C*
015500040119     C                   if        not %open(fivatwwr)
015600040119     C                   open      fivatwwr
015700040119     C                   endif
015800010330     C*
015900040119     C                   clear                   §CTROKVB          7 0
016000040119     C                   clear                   §CTROKVT          7 0
016100040119     C                   clear                   §CTRMO            7 0
016200040119     C                   clear                   §CTRNO            7 0
016300070823     C*
016400070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016500070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016600080916     C                   exsr      inzvar
016700080916     C                   exsr      impvab
016800010330     C*
016900030822     C                   do        *HIVAL
017000010330     C*
017100030822     C                   read      tivin00r                               70
017200010618     C*
017300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017400010618     C                   if        vinflg = '1'
017500010618     C                   eval      flgOk = '1'
017600010618     C                   endif
017700040510     C*
017800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017900040510     C                   z-add     *zeros        wLenVINDTA        4 0
018000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018400040510     C                   z-add     1             wFlgVINDTA
018500040510     C                   leave
018600040510     C                   endif
018700040510     C                   enddo
018800010618     C*
018900040510     C                   if        vindta > *blanks AND
019000040510     C                             wFlgVINDTA = 1
019100010330     C                   add       1             rrnum
019200010330     C*
019300010601     C                   if        *in70 = *off and
019400010330     C                             (vinflg = *blanks
019500010330     C                              or vinflg = '0'
019600010330     C                              or vinflg = '2')
019700010330     C*
019800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019900010711     C                   if        vinflg = *blanks or vinflg = '0'
020000010711     C                   clear                   vinmsg
020100010711     C                   endif
020200010601     C*
020300070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020400070925     C                   eval      vindta  = ' ' + %trim(vindta)
020500070925     C*
020600010330     C                   exsr      inzvar
020700010605     C                   exsr      impvab
020800010601     C*
020900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021000010605     C                   eval      depfil = VABLNP
021100010601     C                   exsr      repfil
021200010601     C                   if        depfil = invfil
021300021025     C                   if        vlrpoi = 999
021400030822     C                   move(P)   invfil        VABFGS
021500021025     C                   else
021600030822     C                   move(P)   vlrpoi        VABFGS
021700021025     C                   endif
021800010601     C*
021900010601     C                   exsr      PREELA
022000010601     C*
022100010604     C* Ebbene...
022200010604     C*
022300040119     C  N31              add       1             §CTROKVB
022400040119     C   32              add       1             §CTRMO
022500040119     C   31              add       1             §CTRNO
022600030822     C                   if        wGiro = 2
022700040510     C* Scrivo sempre il VAT
022800040510     C  N31              exsr      wrivat
022900040510     C* Scrivo il VAB solo a rottura d spedizione
023000040119     C                   if        depspe =  *zeros OR
023100040119     C                             depspe <> vabNSP
023200040119     C  N31              exsr      wrivab
023300040119     C  N31              eval      depspe = vabNSP
023400040119     C                   endif
023500040119     C                   endif
023600010604     C*
023700010604     C                   if        *in31 = *off and
023800010604     C                             *in32 = *off
023900010604     C                   eval      vinflg = '1'
024000010604     C                   else
024100010604     C                   eval      vinflg = '2'
024200010604     C                   endif
024300010604     C                   endif
024400010601     C*
024500010604     C                   endif
024600010604     C*
024700010330     C                   else
024800010330     C                   eval      vinflg = '1'
024900010330     C                   endif
025000010601     C*
025100010601     C  N70              update    tivin000
025200010330     C*
025300030822     C  N70              enddo
025400040119     C*
025500040119     C                   endif                                                  (endif - chkcall)
025600010601     C*
025700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025800010601     C                   if        cntNonEl = *zeros or
025900010601     C                             flgMulti = '0'
026000010330     C* Se non ci sono record con errori ...
026100010601     C                   if        §ctrno = 0 and
026200010604     C                             §ctrmo = 0 and
026300010601     C                             flgStato <> '2'
026400010330     C* ... restituisco esito OK.
026500010330     C                   eval      wrkesito = '0'
026600010330     C                   else
026700040119     C                   if        §ctrokvb > 0
026800010330     C                   eval      wrkesito = '1'
026900010330     C                   else
027000010615     C                   if        flgOk = '0'
027100010615     C                   eval      wrkesito = '2'
027200010615     C                   else
027300010615     C                   eval      wrkesito = '6'
027400010615     C                   endif
027500010330     C                   endif
027600010330     C                   endif
027700010601     C                   else
027800010601     C                   eval      wrkesito = '9'
027900010601     C                   endif
028000010330     C*
028100010330     C                   if        %open(tivin00r)
028200010330     C                   close     tivin00r
028300010330     C                   endif
028400021025     C                   if        %open(fivabwwr)
028500021025     C                   close     fivabwwr
028600010330     C                   endif
028700040512     C                   if        %open(fivatwwr)
028800040512     C                   close     fivatwwr
028900040512     C                   endif
029000010601     C*
029100010601     C                   if        vlrpoi <> 999
029200010601     C                   eval      invfil = vlrpoi
029300010601     C                   endif
029400010330     C*
029500040119     C                   if        §ctrokvb > 0
029600010601     C                             and invfil > *zeros
029700010330     C                   exsr      invio
029800010330     C                   endif
029900010601     C*
030000010618     C                   if        flgGiro = '1'
030100010601     C                   exsr      endela
030200010618     C                   endif
030300010330     C*
030400010330     C                   ENDSR
030500010330     C***
030600040119
030700040119
030800040119
030900040119     C*----------------------------------------------------*
031000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
031100040119     C*----------------------------------------------------*
031200040510     C     WRIVAB        BEGSR
031300040119     C*
031400060331     C* Gestisco l'eventuale rottura x numero spedizione
031500070823     C                   if        VABNSP <> depspe
031600070301     C                   if        wVATNOT_E <> *blanks
031700080125     C                   if        VABCTM = *blanks
031800080125     C                   eval      VABCTM = '7Q'
031900080125     C                   endif
032000070301     C                   endif
032100040119     C                   write     fivab000                                     => scarico il VAB
032200060331     C                   endif
032300040119     C*
032400040119     C                   ENDSR
032500040119
032600040119
032700040119
032800040119     C*----------------------------------------------------*
032900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
033000040119     C*----------------------------------------------------*
033100040510     C     WRIVAT        BEGSR
033200060512     C*
033300060512     C* Inizializzo i campi chiave
033400060512     C                   eval      VATFGS = VABFGS
033500060512     C                   eval      VATAAS = VABAAS
033600060512     C                   eval      VATCCM = VABCCM
033700070823     C                   eval      VATNRS = VABNRS
033800060512     C                   eval      VATNSP = VABNSP
033900060512     C                   eval      VATLNP = VABLNP
034000060331     C*
034100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
034200060331     C                   if        wVATNOT_A <> *blanks
034300060331     C                   eval      VATNOT = wVATNOT_A
034400060331     C                   eval      VATTRC = 'A'
034500060331     C                   write     FIVAT000
034600060331     C                   add       1             §CTROKVT
034700060331     C                   endif
034800060331     C*
034900060331     C                   if        wVATNOT_B <> *blanks
035000060331     C                   eval      VATNOT = wVATNOT_B
035100060331     C                   eval      VATTRC = 'B'
035200060331     C                   write     FIVAT000
035300060331     C                   add       1             §CTROKVT
035400060331     C                   endif
035500060331     C*
035600080625     C                   if        wVATNOT_E <> *blanks
035700060331     C                   eval      VATNOT = wVATNOT_E
035800060331     C                   eval      VATTRC = 'E'
035900080716     C                   write     FIVAT000
036000080716     C                   add       1             §CTROKVT
036100080625     C                   endif
036200120330     C*
036300120330      * Indirizzo e-mail destinatario
036400120330     C                   if        wVATNOT_I <> *blanks
036500120330     C                   eval      VATNOT = wVATNOT_I
036600120330     C                   eval      VATTRC = 'I'
036700120330     C                   write     FIVAT000
036800120330     C                   add       1             §CTROKVT
036900120330     C                   endif
037000120330     C                   if        wVATNOT_J <> *blanks
037100120330     C                   eval      VATNOT = wVATNOT_J
037200120330     C                   eval      VATTRC = 'J'
037300120330     C                   write     FIVAT000
037400120330     C                   add       1             §CTROKVT
037500120330     C                   endif
037600040119     C*
037700040119     C                   ENDSR
037800040119
037900040119
038000040119
038100040119     C*----------------------------------------------------*
038200040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
038300040119     C*----------------------------------------------------*
038400040119     C     PREVAT        BEGSR
038500040119     C*
038600040119     C* Compongo il nome del membro da dare al FIVATWWR
038700040119     C                   eval      parmbr = vlrhdl
038800040119     C                   movel     'M'           parmbr
038900040510     C                   eval      parccm = vlrksc
039000040119     C                   eval      paropz = '1'
039100040119     C* Effettuo la chiamata al CLLE preposto
039200040119     C                   call(e)   'TITVVTC'
039300040119     C                   parm                    parccm
039400040119     C                   parm                    parmbr
039500040119     C                   parm                    paropz
039600040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
039700040119     C                   if        %error
039800040119     C                   movel     '1'           chkcall
039900040119     C                   else
040000040119     C                   movel     '0'           chkcall
040100040119     C                   endif
040200040119     C*
040300040119     C                   ENDSR
040400010601
040500010601
040600010601
040700010330     C*----------------------------------------------------*
040800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
040900010330     C*----------------------------------------------------*
041000040119     C     INZVAR        BEGSR
041100010330     C*
041200030822     C* Inizializzo variabili di wrk
041300020204     C                   Z-ADD     *zeros        Num5_0            5 0
041400060331     C                   MOVEL     *blanks       wVATNOT_A
041500060331     C                   MOVEL     *blanks       wVATNOT_B
041600060331     C                   MOVEL     *blanks       wVATNOT_E
041700120330     C                   MOVEL     *blanks       wVATNOT_I
041800120330     C                   MOVEL     *blanks       wVATNOT_J
041900120330     C                   MOVEL     *blanks       wVATNOT_IJ       70
042000080617     C                   EVAL      wFlgCAS = *blanks
042100030822     C*
042200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
042300030822     C                   CLEAR                   FIVAB000
042400040119     C                   CLEAR                   FIVAT000
042500030822     C                   CLEAR                   SkSplitCSV
042600030822     C                   EVAL      i = 1
042700030822     C                   EVAL      posDa = *zeros
042800030822     C                   EVAL      posA  = *zeros
042900030822     C*
043000030822     C* Reimposto i valori di default
043100030822     C                   EXSR      DEFCAM
043200030822     C*
043300010330     C                   ENDSR
043400010330     C*----------------------------------------------------*
043500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
043600010330     C*----------------------------------------------------*
043700010330     C     DEFCAM        BEGSR
043800030822     C*
043900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
044000030715     C* e delimitatore testo.
044100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
044200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
044300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
044400030822     C*
044500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
044600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
044700030715     C                   SETON                                        50
044800030715     C                   ELSE
044900030715     C                   SETOFF                                       50
045000030715     C                   ENDIF
045100030822     C*
045200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
045300030822     C                   EVAL      posDaDft = 1
045400030822     C                   EVAL      posADft  = 0
045500030822     C                   EVAL      wGiroDft = 0
045600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
045700030822     C                             posDaDft > 0
045800030822     C*
045900030822     C* Gestisco il 1° giro
046000030822     C                   IF        wGiroDft = 0
046100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
046200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
046300030822     C* Incremento il contatore dei "giri"
046400030822     C                   EVAL      wGiroDft = 1
046500030822     C                   ELSE
046600030822     C                   EVAL      posDaDft = posADft
046700030822     C                   ENDIF
046800030822     C* Eseguo lo scan x trovare la fine del campo corrente
046900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
047000030822     C*
047100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
047200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
047300030822     C                   IF        posDaDft > 0 AND
047400030822     C                             posADft  > 0
047500030822     C* NCL
047600030822     C                   IF        %subst(
047700030822     C                             %subst(vlrppt:posDaDft+1:
047800030822     C                             posADft-posDaDft-1):1:3)
047900030822     C                             = 'NCL'
048000030822     C                   EVAL      PiStr=%trim(%subst(
048100030822     C                             %subst(vlrppt:posDaDft+1:
048200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048300030822     C                   EXSR      CHKNUM
048400030822     C                   IF        PiInt=*on
048500030822     C                   Z-ADD     PiVal         VABNCL
048600030822     C                   ENDIF
048700030822     C                   ENDIF
048800030822     C* CCM
048900030822     C                   IF        %subst(
049000030822     C                             %subst(vlrppt:posDaDft+1:
049100030822     C                             posADft-posDaDft-1):1:3)
049200030822     C                             = 'CCM'
049300030822     C                   EVAL      PiStr=%trim(%subst(
049400030822     C                             %subst(vlrppt:posDaDft+1:
049500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049600030822     C                   EXSR      CHKNUM
049700030822     C                   IF        PiInt=*on
049800030822     C                   Z-ADD     PiVal         VABCCM
049900030822     C                   ENDIF
050000030822     C                   ENDIF
050100030822     C* LNP
050200030822     C                   IF        %subst(
050300030822     C                             %subst(vlrppt:posDaDft+1:
050400030822     C                             posADft-posDaDft-1):1:3)
050500030822     C                             = 'LNP'
050600030822     C                   EVAL      PiStr=%trim(%subst(
050700030822     C                             %subst(vlrppt:posDaDft+1:
050800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050900030822     C                   EXSR      CHKNUM
051000030822     C                   IF        PiInt=*on
051100030822     C                   Z-ADD     PiVal         VABLNP
051200030822     C                   ENDIF
051300030822     C                   ENDIF
051400030822     C* NRS
051500030822     C                   IF        %subst(
051600030822     C                             %subst(vlrppt:posDaDft+1:
051700030822     C                             posADft-posDaDft-1):1:3)
051800030822     C                             = 'NRS'
051900030822     C                   EVAL      PiStr=%trim(%subst(
052000030822     C                             %subst(vlrppt:posDaDft+1:
052100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052200030822     C                   EXSR      CHKNUM
052300030822     C                   IF        PiInt=*on
052400030822     C                   Z-ADD     PiVal         VABNRS
052500030822     C                   ENDIF
052600030822     C                   ENDIF
052700030822     C* CTR
052800030822     C                   IF        %subst(
052900030822     C                             %subst(vlrppt:posDaDft+1:
053000030822     C                             posADft-posDaDft-1):1:3)
053100030822     C                             = 'CTR'
053200030822     C                   EVAL      PiStr=%trim(%subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053500030822     C                   EXSR      CHKNUM
053600030822     C                   IF        PiInt=*on
053700030822     C                   Z-ADD     PiVal         VABCTR
053800030822     C                   ENDIF
053900030822     C                   ENDIF
054000030822     C* PKB
054100030822     C                   IF        %subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):1:3)
054400030822     C                             = 'PKB'
054500030822     C                   EVAL      PiStr=%trim(%subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054800030822     C                   EXSR      CHKNUM
054900030822     C                   IF        PiNum=*on
055000030822     C                   Z-ADD     PiVal         VABPKB
055100030822     C                   ENDIF
055200030822     C                   ENDIF
055300030822     C* VLB
055400030822     C                   IF        %subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):1:3)
055700030822     C                             = 'VLB'
055800030822     C                   EVAL      PiStr=%trim(%subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056100030822     C                   EXSR      CHKNUM
056200030822     C                   IF        PiNum=*on
056300030822     C                   Z-ADD     PiVal         VABVLB
056400030822     C                   ENDIF
056500030822     C                   ENDIF
056600030822     C* QFT
056700030822     C                   IF        %subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):1:3)
057000030822     C                             = 'QFT'
057100030822     C                   EVAL      PiStr=%trim(%subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057400030822     C                   EXSR      CHKNUM
057500030822     C                   IF        PiNum=*on
057600030822     C                   Z-ADD     PiVal         VABQFT
057700030822     C                   ENDIF
057800030822     C                   ENDIF
057900030822     C* CBO
058000030822     C                   IF        %subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):1:3)
058300030822     C                             = 'CBO'
058400030822     C                   EVAL      VABCBO=%trim(%subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058700030822     C                   ENDIF
058800030822     C* TSP
058900030822     C                   IF        %subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):1:3)
059200030822     C                             = 'TSP'
059300030822     C                   EVAL      VABTSP=%trim(%subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600030822     C                   ENDIF
059700030822     C* VAS
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'VAS'
060200030822     C                   EVAL      VABVAS=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   ENDIF
060600030822     C* VCA
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'VCA'
061100030822     C                   EVAL      VABVCA=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   ENDIF
061500070925     C* TIC
061600070925     C                   IF        %subst(
061700070925     C                             %subst(vlrppt:posDaDft+1:
061800070925     C                             posADft-posDaDft-1):1:3)
061900070925     C                             = 'TIC'
062000070925     C                   EVAL      VABTIC=%trim(%subst(
062100070925     C                             %subst(vlrppt:posDaDft+1:
062200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300070925     C                   ENDIF
062400030822     C* GCA
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'GCA'
062900030822     C                   EVAL      VABGCA=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   ENDIF
063300030822     C* CTM
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'CTM'
063800030822     C                   EVAL      VABCTM=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   ENDIF
064200030822     C* FFD
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'FFD'
064700030822     C                   EVAL      VABFFD=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   ENDIF
065100030822     C* VAD
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'VAD'
065600030822     C                   EVAL      VABVAD=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* GMA
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'GMA'
066500030822     C                   EVAL      VABGMA=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* GGA
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'GGA'
067400030822     C                   EVAL      VABGGA=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* GVA
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'GVA'
068300030822     C                   EVAL      VABGVA=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* TC1
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'TC1'
069200030822     C                   EVAL      VABTC1=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* TC2
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'TC2'
070100030822     C                   EVAL      VABTC2=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500040714     C* VATTRC
070600040714     C                   IF        %subst(
070700040714     C                             %subst(vlrppt:posDaDft+1:
070800040714     C                             posADft-posDaDft-1):1:3)
070900040714     C                             = 'TRC'
071000040714     C                   EVAL      VATTRC=%trim(%subst(
071100040714     C                             %subst(vlrppt:posDaDft+1:
071200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300040714     C                   ENDIF
071400030822     C* ...
071500030822     C                   ENDIF
071600030822     C                   ENDDO
071700020204     C*
071800010330     C                   ENDSR
071900010607     C*----------------------------------------------------*
072000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
072100010607     C*----------------------------------------------------*
072200010607     C     IMPVAB        BEGSR
072300010607     C*
072400010607     C                   SETOFF                                       3132
072500030515     C*
072600010607     C                   MOVEL     datcor        VABAAS
072700010607     C                   MOVE      datcor        VABMGS
072800010607     C*
072900030715     C**********
073000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073100030715     C**********
073200030715     C                   IF        wGiro = *zeros
073300080716     C*
073400080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
073500080716     C                   EVAL      depvinDTA = vinDTA
073600040113     C*
073700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
073800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
073900040510     C                             %trim(intcol(3))<>*blanks
074000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
074100040113     C                                      %trim(intcol(3))
074200070823     C     ';':CharCSV   XLATE     vindta        vindta
074300040510     C                   ENDIF
074400080626     C*
074500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
074600031201     C     minu:maiu     XLATE     vindta        vindta
074700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074800030715     C                   DOW       posDa <= %len(%trim(vindta))
074900030715     C*
075000030715     C* Gestisco il 1° campo
075100030715     C                   IF        i = 1
075200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075300030715     C                   EVAL      posDa = 1
075400030715     C* Eseguo lo scan x trovare la fine del primo campo
075500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075600030715     C* A questo punto "estraggo" il campo corrente
075700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075800030715     C                                                    (posA-posDa))
075900030715     C* X i campi successivi al 1°
076000030715     C                   ELSE
076100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076200030715     C                   EVAL      posDa = posA + 1
076300030715     C* Eseguo lo scan x trovare la fine del campo corrente
076400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076500030715     C* Gestisco l'ultimo campo
076600030715     C                   IF        posA = *zeros
076700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
076800030715     C* A questo punto "estraggo" il campo corrente
076900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077000030715     C                                                    (posA-posDa))
077100030715     C                   LEAVE
077200030715     C                   ELSE
077300030715     C* A questo punto "estraggo" il campo corrente
077400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077500030715     C                                                    (posA-posDa))
077600030715     C                   ENDIF
077700030715     C                   ENDIF
077800030715     C* Incremento il contatore d campo
077900030715     C                   EVAL      i = i +1
078000030715     C                   ENDDO
078100030715     C*
078200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078300030715     C* delimitazione testo
078400030715     C                   EVAL      i = 1
078500030715     C                   DOW       i < %elem(SkSplitFLD)
078600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
078700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
078800030715     C                   EVAL      i = i + 1
078900030715     C                   ENDDO
079000030715     C                   EVAL      wGiro = 1
079100080716     C* Ripristino la 1° riga originale
079200080716     C                   EVAL      vinDTA = depvinDTA
079300030715     C                   ELSE
079400030715     C                   EVAL      wGiro = 2
079500031204     C**********
079600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
079700031204     C* d separatore campo
079800031204     C**********
079900031204     C                   EVAL      vindta = %trim(vindta)
080000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
080100031204     C                   EVAL      vindta = %subst(vindta:2)
080200031204     C                   ENDDO
080300031204     C*
080400031204     C                   Z-ADD     *zeros        lunghInput        4 0
080500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
080700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
080800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080900031204     C                   ENDDO
081000030715     C**********
081100030715     C* Effettuo lo split del campo dati d input
081200030715     C**********
081300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081400030715     C                   DOW       posDa <= %len(%trim(vindta))
081500030715     C*
081600030715     C* Gestisco il 1° campo
081700030715     C                   IF        i = 1
081800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081900030715     C                   EVAL      posDa = 1
082000030715     C* Eseguo lo scan x trovare la fine del primo campo
082100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082200030715     C* A questo punto "estraggo" il campo corrente
082300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082400030715     C                                                    (posA-posDa))
082500030715     C* X i campi successivi al 1°
082600030715     C                   ELSE
082700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082800030715     C                   EVAL      posDa = posA + 1
082900031204     C* Verifico che nn vi sia il campo nn valorizzato
083000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
083100031204     C* Se campo nn valorizzato skippo al prossimo
083200031204     C                   EVAL      SkSplitCSV(i) = *blanks
083300031204     C                   EVAL      posA  = posA + 1
083400031204     C                   ELSE
083500030715     C* Eseguo lo scan x trovare la fine del campo corrente
083600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083700030715     C* Gestisco l'ultimo campo
083800030715     C                   IF        posA = *zeros
083900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084000030715     C* A questo punto "estraggo" il campo corrente
084100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084200030715     C                                                    (posA-posDa))
084300030715     C                   LEAVE
084400030715     C                   ELSE
084500030715     C* A questo punto "estraggo" il campo corrente
084600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084700030715     C                                                    (posA-posDa))
084800030715     C                   ENDIF
084900031204     C                   ENDIF
085000030715     C                   ENDIF
085100030715     C* Incremento il contatore d campo
085200030715     C                   EVAL      i = i +1
085300030715     C                   ENDDO
085400030715     C*
085500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085600030715     C* delimitazione testo
085700030715     C                   EVAL      i = 1
085800030715     C                   DOW       i < %elem(SkSplitCSV)
085900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
086000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
086100030715     C                   EVAL      i = i + 1
086200030715     C                   ENDDO
086300030715     C*
086400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086500030715     C                   EVAL      i = 1
086600030715     C                   DOW       i < %elem(SkSplitFLD)
086700030715     C*
086800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
086900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
087000030715     C                             SkSplitCSV(i) <> *blanks
087100030721     C*** ATB
087200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
087300030721     C                   EVAL      VABATB = SkSplitCSV(i)
087400030721     C                   ENDIF
087500030715     C*** RSD
087600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
087700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
087800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
087900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
088000030715     C* ==
088100030715     C                   ENDIF
088200030721     C*** RD2
088300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
088400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
088500030721     C                   ENDIF
088600030715     C*** IND
088700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
088800030715     C                   EVAL      VABIND = SkSplitCSV(i)
088900030715     C                   ENDIF
089000030715     C*** LOD
089100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
089200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
089300030715     C                   ENDIF
089400030715     C*** PRD
089500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
089600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
089700030715     C                   ENDIF
089800030721     C*** NZD
089900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
090000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
090100030721     C                   ENDIF
090200030715     C*** RMA
090300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
090400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
090500030715     C                   ENDIF
090600030715     C*** NOT
090700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
090800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
090900030715     C                   ENDIF
091000030715     C*** NT2
091100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
091200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
091300030715     C                   ENDIF
091400030715     C*** VCA
091500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
091600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
091700030715     C                   ENDIF
091800030715     C*** VAS
091900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
092000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
092100030715     C                   ENDIF
092200030715     C*** FFD
092300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
092400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
092500030715     C                   ENDIF
092600030721     C*** GC1
092700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
092800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
092900030721     C                   ENDIF
093000030721     C*** GC2
093100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
093200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
093300030721     C                   ENDIF
093400030721     C*** TSP
093500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
093600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
093700030721     C                   ENDIF
093800040510     C*** CBO
093900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
094000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
094100040510     C                   ENDIF
094200030721     C*** NAS
094300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
094400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
094500030721     C                   ENDIF
094600030721     C*** TIC
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
094800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
094900030721     C                   ENDIF
095000030721     C*** GCA
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
095200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
095300030721     C                   ENDIF
095400030721     C*** XCO
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
095600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030721     C*** CTM
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
096000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
096100030721     C                   ENDIF
096200030721     C*** TCR
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
096400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
096500030721     C                   ENDIF
096600030721     C*** CTS
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
096800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030721     C*** FTM
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
097200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030721     C*** VAD
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
097600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030721     C*** GMA
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
098000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
098100030721     C                   ENDIF
098200030721     C*** GGA
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
098400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** GVA
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
098800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** TC1
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
099200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** TC2
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
099600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** SCL
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
100000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** RMO
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
100400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** NMO
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
100800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*
101100030721     C* Reperisco quindi i campi numerici...
101200040510     C*** NSP
101300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
101400040510     C                   EVAL      PiStr=SkSplitCSV(i)
101500040510     C                   EXSR      CHKNUM
101600040510     C                   IF        PiInt=*on
101700040510     C                   Z-ADD     PiVal         VABNSP
101800040510     C                   ELSE
101900040510     C                   SETON                                        31
102000040510     C                   EVAL      VABNSP = *zeros
102100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
102200040510     C                             + ' ' + 'VABNSP'
102300040510     C                   ENDIF
102400040510     C                   ENDIF
102500071106     C*** RMN
102600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
102700071106     C                   EVAL      PiStr=SkSplitCSV(i)
102800071106     C                   EXSR      CHKNUM
102900071106     C                   IF        PiInt=*on
103000071106     C                   Z-ADD     PiVal         VABRMN
103100071106     C                   ELSE
103200071106     C                   SETON                                        32
103300071106     C                   EVAL      VABRMN = *zeros
103400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
103500071106     C                             + ' ' + 'VABRMN'
103600071106     C                   ENDIF
103700071106     C                   ENDIF
103800030721     C*** CAD
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
104000030721     C                   EVAL      PiStr=SkSplitCSV(i)
104100030721     C                   EXSR      CHKNUM
104200030721     C                   IF        PiInt=*on
104300030721     C                   Z-ADD     PiVal         Num5_0
104400030721     C                   MOVEL(p)  Num5_0        VABCAD
104500030721     C                   ELSE
104600030721     C                   SETON                                        32
104700030721     C                   EVAL      VABCAD = *zeros
104800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104900030721     C                             + ' ' + 'VABCAD'
105000030721     C                   ENDIF
105100030721     C                   ENDIF
105200030721     C*** NCL
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
105400030721     C                   EVAL      PiStr=SkSplitCSV(i)
105500030721     C                   EXSR      CHKNUM
105600030721     C                   IF        PiInt=*on
105700030721     C                   Z-ADD     PiVal         VABNCL
105800030721     C                   ELSE
105900030721     C                   SETON                                        32
106000030721     C                   EVAL      VABNCL = *zeros
106100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106200040122     C                             + ' ' + 'VABNCL'
106300030721     C                   ENDIF
106400030721     C                   ENDIF
106500030721     C*** PKB
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
106700030721     C                   EVAL      PiStr=SkSplitCSV(i)
106800030721     C                   EXSR      CHKNUM
106900030721     C                   IF        PiNum=*on
107000030721     C                   Z-ADD     PiVal         VABPKB
107100030721     C                   ELSE
107200030721     C                   SETON                                        32
107300030721     C                   EVAL      VABPKB = *zeros
107400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107500030721     C                             + ' ' + 'VABPKB'
107600030721     C                   ENDIF
107700030721     C                   ENDIF
107800030721     C*** CAS
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
108000080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
108100080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
108200080617     C                             %trim(SkSplitCSV(i)) <> '0'
108300080125     C                   EVAL      wFlgCAS = '1'
108400080125     C                   ENDIF
108500030721     C                   EVAL      PiStr=SkSplitCSV(i)
108600030721     C                   EXSR      CHKNUM
108700030721     C                   IF        PiNum=*on
108800030721     C                   Z-ADD     PiVal         VABCAS
108900030721     C                   ELSE
109000030721     C                   SETON                                        32
109100030721     C                   EVAL      VABCAS = *zeros
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300030721     C                             + ' ' + 'VABCAS'
109400030721     C                   ENDIF
109500030721     C                   ENDIF
109600030721     C*** CCM
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
109800030721     C                   EVAL      PiStr=SkSplitCSV(i)
109900030721     C                   EXSR      CHKNUM
110000030721     C                   IF        PiInt=*on      AND
110100030721     C                             PiVal<=9999999 AND
110200030721     C                             PiVal>*zeros
110300030721     C                   Z-ADD     PiVal         VABCCM
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      VABCCM = *zeros
110700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110800040510     C                             + ' ' + 'VABCCM'
110900030721     C                   ENDIF
111000030721     C                   ENDIF
111100030721     C*** LNP
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
111300030721     C                   EVAL      PiStr=SkSplitCSV(i)
111400030721     C                   EXSR      CHKNUM
111500030721     C                   IF        PiInt=*on    AND
111600030721     C                             PiVal<=999   AND
111700030721     C                             PiVal>*zeros
111800030721     C                   Z-ADD     PiVal         VABLNP
111900030721     C                   ELSE
112000040715     C                   SETON                                        32
112100030721     C                   EVAL      VABLNP = *zeros
112200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112300040510     C                             + ' ' + 'VABLNP'
112400030721     C                   ENDIF
112500030721     C                   ENDIF
112600030721     C*** NRS
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
112800030721     C                   EVAL      PiStr=SkSplitCSV(i)
112900030721     C                   EXSR      CHKNUM
113000030721     C                   IF        PiInt=*on AND
113100030721     C                             PiVal<=99
113200030721     C                   Z-ADD     PiVal         VABNRS
113300030721     C                   ELSE
113400030721     C                   SETON                                        32
113500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113600030721     C                             + ' ' + 'VABNRS'
113700030721     C                   ENDIF
113800030721     C                   ENDIF
113900030721     C*** LNA
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiInt=*on    AND
114400030721     C                             PiVal<=999   AND
114500030721     C                             PiVal>*zeros
114600030721     C                   Z-ADD     PiVal         VABLNA
114700030721     C                   ELSE
114800030721     C                   SETON                                        32
114900030721     C                   EVAL      VABLNA = *zeros
115000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115100030721     C                             + ' ' + 'VABLNA'
115200030721     C                   ENDIF
115300030721     C                   ENDIF
115400030721     C*** CTR
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiInt=*on  AND
115900030721     C                             PiVal<=999
116000030721     C                   Z-ADD     PiVal         VABCTR
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400030721     C                             + ' ' + 'VABCTR'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** IAS
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiNum=*on
117200030721     C                   Z-ADD     PiVal         VABIAS
117300030721     C                   ELSE
117400030721     C                   SETON                                        32
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600030721     C                             + ' ' + 'VABIAS'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** VLB
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiNum=*on
118400030721     C                   Z-ADD     PiVal         VABVLB
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABVLB'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** QFT
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiNum=*on
119600030721     C                   Z-ADD     PiVal         VABQFT
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000030721     C                             + ' ' + 'VABQFT'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** NCD
120400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
120500030721     C                   EVAL      PiStr=SkSplitCSV(i)
120600030721     C                   EXSR      CHKNUM
120700030721     C                   IF        PiInt=*on      AND
120800030721     C                             PiVal<=9999999
120900030721     C                   Z-ADD     PiVal         VABNCD
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300030721     C                             + ' ' + 'VABNCD'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** NCA
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiInt=*on      AND
122100030721     C                             PiVal<=9999999
122200030721     C                   Z-ADD     PiVal         VABNCA
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABNCA'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** ZNC
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on AND
123400030721     C                             PiVal<=99
123500030721     C                   Z-ADD     PiVal         VABZNC
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABZNC'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** DCR
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on       AND
124700030721     C                             PiVal<=99999999
124800030721     C                   Z-ADD     PiVal         VABDCR
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABDCR'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** HCR
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiInt=*on   AND
126000030721     C                             PiVal<=9999
126100030721     C                   Z-ADD     PiVal         VABHCR
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABHCR'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** VMD
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiNum=*on
127300030721     C                   Z-ADD     PiVal         VABVMD
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABVMD'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** ANT
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiInt=*on        AND
128500030721     C                             PiVal<=999999999
128600030721     C                   Z-ADD     PiVal         VABANT
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABANT'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** CMO
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiInt=*on
129800030721     C                   Z-ADD     PiVal         Num5_0
129900030721     C                   MOVEL(p)  Num5_0        VABCMO
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      VABCMO = *zeros
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABCMO'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700040510     C*
130800040510     C* Reperisco i campi del FIVAT
130900040510     C*** VATNOT
131000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
131100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
131200040510     C                   ENDIF
131300040510     C*** VATTRC
131400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
131500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
131600040510     C                   ENDIF
131700060331     C*** VATNOT_A
131800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
131900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
132000060331     C                   ENDIF
132100060331     C*** VATNOT_B
132200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
132300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
132400060331     C                   ENDIF
132500060331     C*** VATNOT_E
132600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
132700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
132800060331     C                   ENDIF
132900120330     C*** VATNOT_I
133000120330     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
133100120330     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
133200120330     C                   ENDIF
133300120330     C*** VATNOT_J
133400120330     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
133500120330     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
133600120330     C                   ENDIF
133700070823     C*
133800071029     C***  ===>  Gestione campi "particolari" dentro ciclo
133900071204     C*
134000080916     C*** NSP/RMN/RMA
134100080617     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
134200080916     C                   EVAL      VABRMA=SkSplitCSV(i)
134300080716     C                   EVAL      PiStr=SkSplitCSV(i)
134400071204     C                   EXSR      CHKNUM
134500071204     C                   IF        PiInt=*on
134600071204     C                   Z-ADD     PiVal         VABNSP
134700071204     C                   Z-ADD     PiVal         VATNSP
134800071204     C                   Z-ADD     PiVal         VABRMN
134900071204     C                   ELSE
135000080916     C                   SETON                                        31
135100071204     C                   EVAL      VABNSP = *zeros
135200071204     C                   EVAL      VATNSP = *zeros
135300071204     C                   EVAL      VABRMN = *zeros
135400071204     C                   EVAL      vinmsg = %trimr(vinmsg)
135500071204     C                             + ' ' + 'VABNSP VATNSP VABRMN'
135600071204     C                   ENDIF
135700071204     C                   ENDIF
135800080916     C*** CBO
135900080916     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
136000080916     C                   IF        %trim(SkSplitCSV(i)) = 'STA'
136100080916     C                   EVAL      VABCBO = '1'
136200080716     C                   ELSE
136300080916     C                   EVAL      VABCBO = '2'
136400080716     C                   ENDIF
136500080716     C                   ENDIF
136600080916     C*** TIC
136700080916     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
136800080916     C                   EVAL      wFlgCAS = '1'
136900080916     C                   EVAL      VABVCA  = 'EUR'
137000080716     C                   ENDIF
137100120330     C*** NOT_I/NOT_J
137200120330     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
137300120330     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
137400120330     C                   ENDIF
137500070925     C*
137600071029     C***  <===  -----------------------------------------
137700040510     C*
137800030715     C                   ENDIF
137900030715     C                   EVAL      i = i + 1
138000030715     C                   ENDDO
138100070925     C*
138200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
138300080125     C*
138400120330     C                   IF        %trim(wVATNOT_IJ) <> *blanks
138500120330     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
138600120330     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:35)
138700120330     C                   ENDIF
138800070925     C*
138900071029     C***  <===  -----------------------------------------
139000070925     C*
139100070925     C* Considerazioni finali su CBO/CAS
139200080125     C                   IF        wFlgCAS = '1'
139300070925     C                   IF        VABCBO = '1'
139400070925     C                   EVAL      VABCBO = '4'
139500070925     C                   ENDIF
139600070925     C                   IF        VABCBO = '2'
139700070925     C                   EVAL      VABCBO = '6'
139800070925     C                   ENDIF
139900070925     C                   ENDIF
140000070925     C***  <===  ----------------------------
140100040510     C*
140200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
140300040510     C                   IF        VABNSP = *zeros
140400040510     C                   SETON                                        31
140500040510     C                   ENDIF
140600040715     C*
140700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
140800040715     C                   IF        VABLNP = *zeros
140900040715     C                   SETON                                        31
141000040715     C                   ENDIF
141100040510     C*
141200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
141300040510     C                   IF        VABRMN = *zeros
141400040510     C                   Z-ADD     VABNSP        VABRMN
141500040510     C                   ENDIF
141600040510     C*
141700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
141800040510     C                   IF        VABRMA = *blanks
141900040714     C                   MOVEL     VABRMN        VABRMA
142000040510     C                   ENDIF
142100040128     C*
142200040510     C* Se provincia nn valorizzata la reperisco
142300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
142400040510     C                   IF        VABPRD = *blanks
142500040128     C                   CLEAR                   TISI95DS
142600040128     C                   EVAL      I95TCN = '3'
142700040128     C                   Z-ADD     datcor        I95DAT
142800040128     C                   EVAL      I95NAR = VABNZD
142900040128     C                   EVAL      I95CAP = VABCAD
143000040128     C                   EVAL      I95LOC = VABLOD
143100040128     C                   CALL      'TISI95R'
143200040128     C                   PARM                    TISI95DS
143300040128     C                   EVAL      VABPRD = O95PRV
143400040510     C                   ENDIF
143500020204     C*
143600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
143700020204     C                   EXSR      CHKIMPDIV
143800030715     C                   ENDIF
143900020204     C*
144000010607     C                   ENDSR
144100010607     C*----------------------------------------------------*
144200010601
144300020204
144400020204     C*----------------------------------------------------*
144500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
144600020204     C*----------------------------------------------------*
144700020204     C     CHKIMPDIV     BEGSR
144800020204     C*
144900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
145000020204     C                   Z-ADD     *zeros        wrkDec            9 9
145100020204     C*
145200020204     C* Come prima cosa effettuo considerazioni sulla divisa
145300020204     C                   IF        vabIAS > *zeros
145400020204     C                   IF        vabVAS <> 'EUR'
145500020204     C                   EVAL      vabVAS =  'ITL'
145600020204     C                   ENDIF
145700020204     C                   ENDIF
145800020204     C*
145900020204     C                   IF        vabCAS > *zeros
146000020204     C                   IF        vabVCA <> 'EUR'
146100020204     C                   EVAL      vabVCA =  'ITL'
146200020204     C                   ENDIF
146300020204     C                   ENDIF
146400020204     C*
146500020204     C                   IF        vabVMD > *zeros
146600020204     C                   IF        vabVAD <> 'EUR'
146700020204     C                   EVAL      vabVAD =  'ITL'
146800020204     C                   ENDIF
146900020204     C                   ENDIF
147000020204     C*
147100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
147200020204     C                   Z-ADD     vabIAS        wrkDec
147300020204     C                   IF        wrkDec > *zeros
147400020204     C                   IF        vabVAS = 'ITL'
147500020204     C                   EVAL      vabIAS = *zeros
147600020204     C                   ENDIF
147700020204     C                   ENDIF
147800020204     C*
147900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
148000020204     C                   Z-ADD     vabCAS        wrkDec
148100020204     C                   IF        wrkDec > *zeros
148200020204     C                   IF        vabVCA = 'ITL'
148300020204     C                   EVAL      vabCAS = *zeros
148400020204     C                   ENDIF
148500020204     C                   ENDIF
148600020204     C*
148700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
148800020204     C                   Z-ADD     vabVMD        wrkDec
148900020204     C                   IF        wrkDec > *zeros
149000020204     C                   IF        vabVAD = 'ITL'
149100020204     C                   EVAL      vabVMD = *zeros
149200020204     C                   ENDIF
149300020204     C                   ENDIF
149400020204     C*
149500020204     C                   ENDSR
149600020204     C***
149700080617
149800080617
149900080617
150000080617     C*----------------------------------------------------*
150100080617     C*  CONTROLLO NUMERICITA' CAMPI
150200080617     C*----------------------------------------------------*
150300080617     C     CHKNUM        BEGSR
150400080617     C*
150500080617     C                   IF        PiDecChr = *blanks
150600080617     C                   EVAL      PiDecChr = CharNUM
150700080617     C                   ENDIF
150800080617     C*
150900080617     C                   callp     UBISNUM_Check(PiStr
151000080617     C                                          :PiDecChr
151100080617     C                                          :PiVal
151200080617     C                                          :PiNum
151300080617     C                                          :PiInt)
151400080617     C*
151500080617     C                   ENDSR
151600080617     C***
151700010330
151800010601
151900010601
152000010601
152100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
152200010601     C     repfil        BEGSR
152300010601     C*
152400010601     C                   if        invfil = *zeros and
152500010601     C                             depfil > *zeros and
152600010629     C                             (vinflg = *blanks or
152700010629     C                              vinflg = *zeros)
152800010601     C
152900010601     C                   eval      invfil = depfil
153000010601     C                   endif
153100010601     C*
153200010601     C                   if        depfil <> invfil and
153300010601     C                             invfil > *zeros
153400010601     C                   eval      flgMulti = '1'
153500010601     C                   if        vinflg = *blanks
153600010601     C                   add       1             cntNonEl
153700010601     C                   endif
153800010601     C                   endif
153900010601     C*
154000010601     C                   if        vinflg = '2'
154100010601     C                   eval      flgStato = '2'
154200010601     C                   endif
154300010601     C*
154400010601     C                   ENDSR
154500010601     C***
154600010601
154700010601
154800010601
154900010330
155000040119      /TITLE Invio dei dati al punto operativo.
155100040119     C     invio         BEGSR
155200040119     C*
155300040119     C* 1° invio FIVAT
155400040119     C                   reset                   dscmz
155500040119     C                   move      vlrpoi        cmzdst
155600040119     C                   eval      cmzfld = 'FIVATWWR'
155700040119     C                   eval      cmzmbd = vlrhdl
155800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
155900040119     C***                if        prmfir = *blanks
156000040119     C                   eval      cmzfla = 'FIVAT00F'
156100040119     C                   eval      cmzmba = 'FIVAT00F'
156200040119     C***                else
156300040119     C***                eval      cmzfla = prmfir
156400040119     C***                eval      cmzmba = prmfir
156500040119     C***                endif
156600040119     C                   eval      cmznrr = *zeros
156700040119     C                   move      §ctrokvt      cmznrr
156800040119     C                   eval      cmzlba = vlrfl1
156900040119     C                   call(e)   'TIS711C'
157000040119     C                   parm                    dscmz
157100040119     C                   parm      *blanks       esito
157200040119     C                   if        %error
157300040119     C                             or cmzerr = '1'
157400040119     C                             or esito  = '1'
157500040119     C                   eval      wrkesito = '3'
157600040119     C                   else
157700040119     C*
157800040119     C* 2° invio FIVAB
157900040119     C                   reset                   dscmz
158000040119     C                   move      vlrpoi        cmzdst
158100040119     C                   eval      cmzfld = vlrfou
158200040119     C                   eval      cmzmbd = vlrhdl
158300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
158400040119     C***                if        prmfir = *blanks
158500040119     C                   eval      cmzfla = 'FIVAB00F'
158600040119     C                   eval      cmzmba = 'FIVAB00F'
158700040119     C***                else
158800040119     C***                eval      cmzfla = prmfir
158900040119     C***                eval      cmzmba = prmfir
159000040119     C***                endif
159100040119     C                   eval      cmznrr = *zeros
159200040119     C                   move      §ctrokvb      cmznrr
159300040119     C                   eval      cmzlba = vlrfl1
159400040119     C                   call(e)   'TIS711C'
159500040119     C                   parm                    dscmz
159600040119     C                   parm      *blanks       esito
159700040119     C                   if        %error
159800040119     C                             or cmzerr = '1'
159900040119     C                             or esito  = '1'
160000040119     C                   eval      wrkesito = '3'
160100040119     C                   endif
160200040119     C                   endif
160300040119     C*
160400040119     C                   ENDSR
160500040119     C***
160600010601
160700010601
160800010601
160900010601
161000010601      /TITLE Invio dei dati al punto operativo.
161100010601     C     opeini        BEGSR
161200010601     C*
161300010601     C* Inizializzo flag e contatori operativi
161400010601     C                   movel     '0'           flgGiro           1
161500010601     C                   movel     '0'           flgMulti          1
161600010601     C                   movel     '1'           flgStato          1
161700010615     C                   movel     '0'           flgOk             1
161800010601     C                   z-add     *zeros        cntNonEl         10 0
161900010601     C                   z-add     *zeros        depfil            3 0
162000010601     C                   z-add     *zeros        invfil            3 0
162100010601     C*
162200010601     C                   ENDSR
162300010601     C***
162400070326
162500070326
162600070326
162700070326
162800080916     C     *pssr         BEGSR
162900070329     C*
163000080916     C                   if        %open(tivin00r)
163100080916     C                   close     tivin00r
163200080916     C                   endif
163300080916     C                   if        %open(fivabwwr)
163400080916     C                   close     fivabwwr
163500080916     C                   endif
163600080916     C                   if        %open(fivatwwr)
163700080916     C                   close     fivatwwr
163800080916     C                   endif
163900070326     C*
164000070326     C* Effettuo la chiamata al CLLE preposto
164100080916     C                   call(e)   'TITVVTC'
164200080916     C                   parm                    parccm
164300080916     C                   parm                    parmbr
164400080916     C                   parm      '2'           paropz
164500070326     C*
164600080916     C                   eval      wrkesito = '2'
164700070404     C*
164800080916     C                   seton                                        LR
164900070326     C*
165000080916     C                   ENDSR     '*CANCL'
165100070326     C***
165200070326
165300070326
165400010330
165500010330
165600000613     C     *inzsr        BEGSR
165700990910     C*
165800990910     C     *entry        plist
165900990920     C                   parm                    tivlrds
166000990921     C                   parm      wrkesito      esito
166100000724     C                   parm                    prmlit
166200000710     C                   parm                    prmfir
166300010330     C*
166400010330     C* CALCOLA LA DATA CORRENTE
166500010330     C                   time                    wn14             14 0
166600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
166700010330     C                   z-add     wn8           g08dat
166800010330     C                   z-add     *zeros        g08inv
166900010330     C                   movel     '0'           g08err
167000010330     C                   call      'XSRDA8'
167100010330     C                   parm                    wlbda8
167200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
167300000613     C*
167400000613     C                   ENDSR
167500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
167600080916VABRSD;VABIND;VABLOD;VABPRD;VABCAD;VABRD2;VABNOT;W_VABCBO;W_VABRMN;
167700120330VABNCL;VABPKB;VABNT2;W_VABTIC;VABCAS;VATNOT_E;VABTC1;VABTSP;W_VATNOT_I
167800080715
