000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
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
003000030715     D*------------------
003100030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003200030715     D*------------------
003300030715     D SkSplitFLD      S             10    DIM(100)
003400030715     D SkSplitCSV      S            256    DIM(100)
003500030715     D CharCSV         S              1
003600030715     D CharTXT         S              1
003700030715     D CharNUM         S              1
003800030715     D posDa           S              3  0 INZ(*zeros)
003900030715     D posA            S              3  0 INZ(*zeros)
004000030715     D i               s              3  0 INZ(1)
004100030715     D wGiro           s              1  0 INZ(*zeros)
004200070823     D wVATNOT_A       s                   LIKE(VATNOT)
004300060331     D wVATNOT_B       s                   LIKE(VATNOT)
004400060331     D wVATNOT_E       s                   LIKE(VATNOT)
004500030822     D*------------------
004600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004700030822     D*------------------
004800030822     D posDaDft        S              3  0 INZ(*zeros)
004900030822     D posADft         S              3  0 INZ(*zeros)
005000030822     D j               s              3  0 INZ(1)
005100030822     D wGiroDft        s              1  0 INZ(*zeros)
005200010330     D*------------------
005300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005400010330     D*------------------
005500010330     D WLBDA8          DS                  INZ
005600010330     D  G08DAT                 1      8  0
005700010330     D  G08INV                 9     16  0
005800010330     D  G08ERR                17     17
005900010330     D  G08TGI                18     22  0
006000031201     D*------------------
006100031201     D* Costanti
006200031201     D*------------------
006300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006500010330
006600010330
006700010330
006800000913     C                   reset                   rrnum
006900990921     C                   reset                   esito
007000990921     C                   reset                   wrkesito
007100010601     C*
007200010601     C                   exsr      opeini
007300010605     C                   exsr      rwvab
007400010601     C*
007500010601     C                   seton                                        lr
007600010601
007700010601
007800010601
007900010601     C*--------------------------------------------------------
008000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008100010601     C*--------------------------------------------------------
008200010601     C     PREELA        BEGSR
008300010601     C*
008400010601     C* SE OCCORRE SPEDIRE IN FILIALE
008500010601     C                   if        invfil <> *zeros and
008600010601     C                             flgGiro = '0'
008700010601     C*
008800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008900010601     C                   eval      flgGiro = '1'
009000010601     C*
009100010601     C                   endif
009200010601     C*
009300010601     C                   ENDSR
009400010601     C***
009500010601
009600010601
009700010601
009800010601
009900010601     C*--------------------------------------------------------
010000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010100010601     C*--------------------------------------------------------
010200010601     C     ENDELA        BEGSR
010300040119     C*
010400040119     C* Effettuo la chiamata al CLLE preposto
010500040119     C                   call(e)   'TITVVTC'
010600040510     C                   parm                    parccm
010700040119     C                   parm                    parmbr
010800040119     C                   parm      '2'           paropz
010900050201     C*
011000050201     C* Effettuo lancio TISI95 solo x chiusura
011100050201     C                   CLEAR                   TISI95DS
011200050201     C                   EVAL      I95TLA = 'C'
011300050201     C                   CALL      'TISI95R'
011400050201     C                   PARM                    TISI95DS
011500000616     C*
011600010601     C                   ENDSR
011700010601     C***
011800000613
011900010601
012000010601
012100010330     C*--------------------------------------------------------
012200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
012300010330     C*--------------------------------------------------------
012400010605     C     RWVAB         BEGSR
012500010330     C*
012600010330     C                   if        not %open(tivin00r)
012700010330     C                   open      tivin00r
012800010330     C                   endif
012900021025     C                   if        not %open(fivabwwr)
013000021025     C                   open      fivabwwr
013100010330     C                   endif
013200040119     C*
013300040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013400040119     C                   exsr      prevat
013500040119     C*
013600040119     C                   if        chkcall = '0'
013700040119     C*
013800040119     C                   if        not %open(fivatwwr)
013900040119     C                   open      fivatwwr
014000040119     C                   endif
014100010330     C*
014200040119     C                   clear                   §CTROKVB          7 0
014300040119     C                   clear                   §CTROKVT          7 0
014400040119     C                   clear                   §CTRMO            7 0
014500040119     C                   clear                   §CTRNO            7 0
014600070823     C*
014700070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
014800070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
014900070823     C                   exsr      inzvar
015000070823     C                   exsr      impvab
015100010330     C*
015200030822     C                   do        *HIVAL
015300010330     C*
015400030822     C                   read      tivin00r                               70
015500010618     C*
015600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015700010618     C                   if        vinflg = '1'
015800010618     C                   eval      flgOk = '1'
015900010618     C                   endif
016000040510     C*
016100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016200040510     C                   z-add     *zeros        wLenVINDTA        4 0
016300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
016500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016700040510     C                   z-add     1             wFlgVINDTA
016800040510     C                   leave
016900040510     C                   endif
017000040510     C                   enddo
017100010618     C*
017200040510     C                   if        vindta > *blanks AND
017300040510     C                             wFlgVINDTA = 1
017400010330     C                   add       1             rrnum
017500010330     C*
017600010601     C                   if        *in70 = *off and
017700010330     C                             (vinflg = *blanks
017800010330     C                              or vinflg = '0'
017900010330     C                              or vinflg = '2')
018000010330     C*
018100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018200010711     C                   if        vinflg = *blanks or vinflg = '0'
018300010711     C                   clear                   vinmsg
018400010711     C                   endif
018500010601     C*
018600010330     C                   exsr      inzvar
018700010605     C                   exsr      impvab
018800010601     C*
018900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019000010605     C                   eval      depfil = VABLNP
019100010601     C                   exsr      repfil
019200010601     C                   if        depfil = invfil
019300021025     C                   if        vlrpoi = 999
019400030822     C                   move(P)   invfil        VABFGS
019500021025     C                   else
019600030822     C                   move(P)   vlrpoi        VABFGS
019700021025     C                   endif
019800010601     C*
019900010601     C                   exsr      PREELA
020000010601     C*
020100010604     C* Ebbene...
020200010604     C*
020300040119     C  N31              add       1             §CTROKVB
020400040119     C   32              add       1             §CTRMO
020500040119     C   31              add       1             §CTRNO
020600030822     C                   if        wGiro = 2
020700040510     C* Scrivo sempre il VAT
020800040510     C  N31              exsr      wrivat
020900040510     C* Scrivo il VAB solo a rottura d spedizione
021000040119     C                   if        depspe =  *zeros OR
021100040119     C                             depspe <> vabNSP
021200040119     C  N31              exsr      wrivab
021300040119     C  N31              eval      depspe = vabNSP
021400040119     C                   endif
021500040119     C                   endif
021600010604     C*
021700010604     C                   if        *in31 = *off and
021800010604     C                             *in32 = *off
021900010604     C                   eval      vinflg = '1'
022000010604     C                   else
022100010604     C                   eval      vinflg = '2'
022200010604     C                   endif
022300010604     C                   endif
022400010601     C*
022500010604     C                   endif
022600010604     C*
022700010330     C                   else
022800010330     C                   eval      vinflg = '1'
022900010330     C                   endif
023000010601     C*
023100010601     C  N70              update    tivin000
023200010330     C*
023300030822     C  N70              enddo
023400040119     C*
023500040119     C                   endif                                                  (endif - chkcall)
023600010601     C*
023700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023800010601     C                   if        cntNonEl = *zeros or
023900010601     C                             flgMulti = '0'
024000010330     C* Se non ci sono record con errori ...
024100010601     C                   if        §ctrno = 0 and
024200010604     C                             §ctrmo = 0 and
024300010601     C                             flgStato <> '2'
024400010330     C* ... restituisco esito OK.
024500010330     C                   eval      wrkesito = '0'
024600010330     C                   else
024700040119     C                   if        §ctrokvb > 0
024800010330     C                   eval      wrkesito = '1'
024900010330     C                   else
025000010615     C                   if        flgOk = '0'
025100010615     C                   eval      wrkesito = '2'
025200010615     C                   else
025300010615     C                   eval      wrkesito = '6'
025400010615     C                   endif
025500010330     C                   endif
025600010330     C                   endif
025700010601     C                   else
025800010601     C                   eval      wrkesito = '9'
025900010601     C                   endif
026000010330     C*
026100010330     C                   if        %open(tivin00r)
026200010330     C                   close     tivin00r
026300010330     C                   endif
026400021025     C                   if        %open(fivabwwr)
026500021025     C                   close     fivabwwr
026600010330     C                   endif
026700040512     C                   if        %open(fivatwwr)
026800040512     C                   close     fivatwwr
026900040512     C                   endif
027000010601     C*
027100010601     C                   if        vlrpoi <> 999
027200010601     C                   eval      invfil = vlrpoi
027300010601     C                   endif
027400010330     C*
027500040119     C                   if        §ctrokvb > 0
027600010601     C                             and invfil > *zeros
027700010330     C                   exsr      invio
027800010330     C                   endif
027900010601     C*
028000010618     C                   if        flgGiro = '1'
028100010601     C                   exsr      endela
028200010618     C                   endif
028300010330     C*
028400010330     C                   ENDSR
028500010330     C***
028600040119
028700040119
028800040119
028900040119     C*----------------------------------------------------*
029000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
029100040119     C*----------------------------------------------------*
029200040510     C     WRIVAB        BEGSR
029300040119     C*
029400060331     C* Gestisco l'eventuale rottura x numero spedizione
029500070823     C                   if        VABNSP <> depspe
029600070301     C                   if        wVATNOT_E <> *blanks
029700070301     C                   eval      VABCTM = '7Q'
029800070301     C                   endif
029900040119     C                   write     fivab000                                     => scarico il VAB
030000060331     C                   endif
030100040119     C*
030200040119     C                   ENDSR
030300040119
030400040119
030500040119
030600040119     C*----------------------------------------------------*
030700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
030800040119     C*----------------------------------------------------*
030900040510     C     WRIVAT        BEGSR
031000060512     C*
031100060512     C* Inizializzo i campi chiave
031200060512     C                   eval      VATFGS = VABFGS
031300060512     C                   eval      VATAAS = VABAAS
031400060512     C                   eval      VATCCM = VABCCM
031500070823     C                   eval      VATNRS = VABNRS
031600060512     C                   eval      VATNSP = VABNSP
031700060512     C                   eval      VATLNP = VABLNP
031800040714     C*
031900040714     C* Solo se VATNOT valorizzato scarico il buffer
032000040714     C                   if        VATNOT <> *blanks
032100040119     C*
032200040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
032300040510     C                   if        VATTRC = *blanks
032400040510     C                   eval      VATTRC = 'E'
032500040510     C                   endif
032600040510     C                   write     FIVAT000
032700040119     C                   add       1             §CTROKVT
032800040714     C*
032900040714     C                   endif
033000060331     C*
033100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
033200060331     C                   if        wVATNOT_A <> *blanks
033300060331     C                   eval      VATNOT = wVATNOT_A
033400060331     C                   eval      VATTRC = 'A'
033500060331     C                   write     FIVAT000
033600060331     C                   add       1             §CTROKVT
033700060331     C                   endif
033800060331     C*
033900060331     C                   if        wVATNOT_B <> *blanks
034000060331     C                   eval      VATNOT = wVATNOT_B
034100060331     C                   eval      VATTRC = 'B'
034200060331     C                   write     FIVAT000
034300060331     C                   add       1             §CTROKVT
034400060331     C                   endif
034500060331     C*
034600060331     C                   if        wVATNOT_E <> *blanks
034700060331     C                   eval      VATNOT = wVATNOT_E
034800060331     C                   eval      VATTRC = 'E'
034900060331     C                   write     FIVAT000
035000060331     C                   add       1             §CTROKVT
035100060331     C                   endif
035200040119     C*
035300040119     C                   ENDSR
035400040119
035500040119
035600040119
035700040119     C*----------------------------------------------------*
035800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
035900040119     C*----------------------------------------------------*
036000040119     C     PREVAT        BEGSR
036100040119     C*
036200040119     C* Compongo il nome del membro da dare al FIVATWWR
036300040119     C                   eval      parmbr = vlrhdl
036400040119     C                   movel     'M'           parmbr
036500040510     C                   eval      parccm = vlrksc
036600040119     C                   eval      paropz = '1'
036700040119     C* Effettuo la chiamata al CLLE preposto
036800040119     C                   call(e)   'TITVVTC'
036900040119     C                   parm                    parccm
037000040119     C                   parm                    parmbr
037100040119     C                   parm                    paropz
037200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
037300040119     C                   if        %error
037400040119     C                   movel     '1'           chkcall
037500040119     C                   else
037600040119     C                   movel     '0'           chkcall
037700040119     C                   endif
037800040119     C*
037900040119     C                   ENDSR
038000010601
038100010601
038200010601
038300010330     C*----------------------------------------------------*
038400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
038500010330     C*----------------------------------------------------*
038600040119     C     INZVAR        BEGSR
038700010330     C*
038800030822     C* Inizializzo variabili di wrk
038900020204     C                   Z-ADD     *zeros        Num5_0            5 0
039000060331     C                   MOVEL     *blanks       wVATNOT_A
039100060331     C                   MOVEL     *blanks       wVATNOT_B
039200060331     C                   MOVEL     *blanks       wVATNOT_E
039300030822     C*
039400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
039500030822     C                   CLEAR                   FIVAB000
039600040119     C                   CLEAR                   FIVAT000
039700030822     C                   CLEAR                   SkSplitCSV
039800030822     C                   EVAL      i = 1
039900030822     C                   EVAL      posDa = *zeros
040000030822     C                   EVAL      posA  = *zeros
040100030822     C*
040200030822     C* Reimposto i valori di default
040300030822     C                   EXSR      DEFCAM
040400030822     C*
040500010330     C                   ENDSR
040600010330     C*----------------------------------------------------*
040700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
040800010330     C*----------------------------------------------------*
040900010330     C     DEFCAM        BEGSR
041000030822     C*
041100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
041200030715     C* e delimitatore testo.
041300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
041400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
041500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
041600030822     C*
041700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
041800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
041900030715     C                   SETON                                        50
042000030715     C                   ELSE
042100030715     C                   SETOFF                                       50
042200030715     C                   ENDIF
042300030822     C*
042400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
042500030822     C                   EVAL      posDaDft = 1
042600030822     C                   EVAL      posADft  = 0
042700030822     C                   EVAL      wGiroDft = 0
042800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
042900030822     C                             posDaDft > 0
043000070907     C*
043100070907     C* Default fissi
043200070907     C                   EVAL      VABCTM = '7Q'
043300030822     C*
043400030822     C* Gestisco il 1° giro
043500030822     C                   IF        wGiroDft = 0
043600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
043700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
043800030822     C* Incremento il contatore dei "giri"
043900030822     C                   EVAL      wGiroDft = 1
044000030822     C                   ELSE
044100030822     C                   EVAL      posDaDft = posADft
044200030822     C                   ENDIF
044300030822     C* Eseguo lo scan x trovare la fine del campo corrente
044400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
044500030822     C*
044600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
044700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
044800030822     C                   IF        posDaDft > 0 AND
044900030822     C                             posADft  > 0
045000030822     C* NCL
045100030822     C                   IF        %subst(
045200030822     C                             %subst(vlrppt:posDaDft+1:
045300030822     C                             posADft-posDaDft-1):1:3)
045400030822     C                             = 'NCL'
045500030822     C                   EVAL      PiStr=%trim(%subst(
045600030822     C                             %subst(vlrppt:posDaDft+1:
045700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045800030822     C                   EXSR      CHKNUM
045900030822     C                   IF        PiInt=*on
046000030822     C                   Z-ADD     PiVal         VABNCL
046100030822     C                   ENDIF
046200030822     C                   ENDIF
046300030822     C* CCM
046400030822     C                   IF        %subst(
046500030822     C                             %subst(vlrppt:posDaDft+1:
046600030822     C                             posADft-posDaDft-1):1:3)
046700030822     C                             = 'CCM'
046800030822     C                   EVAL      PiStr=%trim(%subst(
046900030822     C                             %subst(vlrppt:posDaDft+1:
047000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047100030822     C                   EXSR      CHKNUM
047200030822     C                   IF        PiInt=*on
047300030822     C                   Z-ADD     PiVal         VABCCM
047400030822     C                   ENDIF
047500030822     C                   ENDIF
047600030822     C* LNP
047700030822     C                   IF        %subst(
047800030822     C                             %subst(vlrppt:posDaDft+1:
047900030822     C                             posADft-posDaDft-1):1:3)
048000030822     C                             = 'LNP'
048100030822     C                   EVAL      PiStr=%trim(%subst(
048200030822     C                             %subst(vlrppt:posDaDft+1:
048300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048400030822     C                   EXSR      CHKNUM
048500030822     C                   IF        PiInt=*on
048600030822     C                   Z-ADD     PiVal         VABLNP
048700030822     C                   ENDIF
048800030822     C                   ENDIF
048900030822     C* NRS
049000030822     C                   IF        %subst(
049100030822     C                             %subst(vlrppt:posDaDft+1:
049200030822     C                             posADft-posDaDft-1):1:3)
049300030822     C                             = 'NRS'
049400030822     C                   EVAL      PiStr=%trim(%subst(
049500030822     C                             %subst(vlrppt:posDaDft+1:
049600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049700030822     C                   EXSR      CHKNUM
049800030822     C                   IF        PiInt=*on
049900030822     C                   Z-ADD     PiVal         VABNRS
050000030822     C                   ENDIF
050100030822     C                   ENDIF
050200030822     C* CTR
050300030822     C                   IF        %subst(
050400030822     C                             %subst(vlrppt:posDaDft+1:
050500030822     C                             posADft-posDaDft-1):1:3)
050600030822     C                             = 'CTR'
050700030822     C                   EVAL      PiStr=%trim(%subst(
050800030822     C                             %subst(vlrppt:posDaDft+1:
050900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051000030822     C                   EXSR      CHKNUM
051100030822     C                   IF        PiInt=*on
051200030822     C                   Z-ADD     PiVal         VABCTR
051300030822     C                   ENDIF
051400030822     C                   ENDIF
051500030822     C* PKB
051600030822     C                   IF        %subst(
051700030822     C                             %subst(vlrppt:posDaDft+1:
051800030822     C                             posADft-posDaDft-1):1:3)
051900030822     C                             = 'PKB'
052000030822     C                   EVAL      PiStr=%trim(%subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052300030822     C                   EXSR      CHKNUM
052400030822     C                   IF        PiNum=*on
052500030822     C                   Z-ADD     PiVal         VABPKB
052600030822     C                   ENDIF
052700030822     C                   ENDIF
052800030822     C* VLB
052900030822     C                   IF        %subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):1:3)
053200030822     C                             = 'VLB'
053300030822     C                   EVAL      PiStr=%trim(%subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053600030822     C                   EXSR      CHKNUM
053700030822     C                   IF        PiNum=*on
053800030822     C                   Z-ADD     PiVal         VABVLB
053900030822     C                   ENDIF
054000030822     C                   ENDIF
054100030822     C* QFT
054200030822     C                   IF        %subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):1:3)
054500030822     C                             = 'QFT'
054600030822     C                   EVAL      PiStr=%trim(%subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054900030822     C                   EXSR      CHKNUM
055000030822     C                   IF        PiNum=*on
055100030822     C                   Z-ADD     PiVal         VABQFT
055200030822     C                   ENDIF
055300030822     C                   ENDIF
055400030822     C* CBO
055500030822     C                   IF        %subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):1:3)
055800030822     C                             = 'CBO'
055900030822     C                   EVAL      VABCBO=%trim(%subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200030822     C                   ENDIF
056300030822     C* TSP
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'TSP'
056800030822     C                   EVAL      VABTSP=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   ENDIF
057200030822     C* VAS
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'VAS'
057700030822     C                   EVAL      VABVAS=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   ENDIF
058100030822     C* VCA
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'VCA'
058600030822     C                   EVAL      VABVCA=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   ENDIF
059000030822     C* GCA
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'GCA'
059500030822     C                   EVAL      VABGCA=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   ENDIF
059900030822     C* CTM
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'CTM'
060400030822     C                   EVAL      VABCTM=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   ENDIF
060800030822     C* FFD
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'FFD'
061300030822     C                   EVAL      VABFFD=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   ENDIF
061700030822     C* VAD
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'VAD'
062200030822     C                   EVAL      VABVAD=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   ENDIF
062600030822     C* GMA
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'GMA'
063100030822     C                   EVAL      VABGMA=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   ENDIF
063500030822     C* GGA
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'GGA'
064000030822     C                   EVAL      VABGGA=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   ENDIF
064400030822     C* GVA
064500030822     C                   IF        %subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):1:3)
064800030822     C                             = 'GVA'
064900030822     C                   EVAL      VABGVA=%trim(%subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065200030822     C                   ENDIF
065300030822     C* TC1
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'TC1'
065800030822     C                   EVAL      VABTC1=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   ENDIF
066200030822     C* TC2
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'TC2'
066700030822     C                   EVAL      VABTC2=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   ENDIF
067100040714     C* VATTRC
067200040714     C                   IF        %subst(
067300040714     C                             %subst(vlrppt:posDaDft+1:
067400040714     C                             posADft-posDaDft-1):1:3)
067500040714     C                             = 'TRC'
067600040714     C                   EVAL      VATTRC=%trim(%subst(
067700040714     C                             %subst(vlrppt:posDaDft+1:
067800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900040714     C                   ENDIF
068000030822     C* ...
068100030822     C                   ENDIF
068200030822     C                   ENDDO
068300020204     C*
068400010330     C                   ENDSR
068500010607     C*----------------------------------------------------*
068600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
068700010607     C*----------------------------------------------------*
068800010607     C     IMPVAB        BEGSR
068900010607     C*
069000010607     C                   SETOFF                                       3132
069100030515     C*
069200010607     C                   MOVEL     datcor        VABAAS
069300010607     C                   MOVE      datcor        VABMGS
069400010607     C*
069500030715     C**********
069600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
069700030715     C**********
069800030715     C                   IF        wGiro = *zeros
069900040113     C*
070000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
070100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
070200040510     C                             %trim(intcol(3))<>*blanks
070300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
070400040113     C                                      %trim(intcol(3))
070500070823     C     ';':CharCSV   XLATE     vindta        vindta
070600040510     C                   ENDIF
070700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
070800031201     C     minu:maiu     XLATE     vindta        vindta
070900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
071000030715     C                   DOW       posDa <= %len(%trim(vindta))
071100030715     C*
071200030715     C* Gestisco il 1° campo
071300030715     C                   IF        i = 1
071400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
071500030715     C                   EVAL      posDa = 1
071600030715     C* Eseguo lo scan x trovare la fine del primo campo
071700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071800030715     C* A questo punto "estraggo" il campo corrente
071900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
072000030715     C                                                    (posA-posDa))
072100030715     C* X i campi successivi al 1°
072200030715     C                   ELSE
072300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
072400030715     C                   EVAL      posDa = posA + 1
072500030715     C* Eseguo lo scan x trovare la fine del campo corrente
072600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072700030715     C* Gestisco l'ultimo campo
072800030715     C                   IF        posA = *zeros
072900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
073000030715     C* A questo punto "estraggo" il campo corrente
073100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073200030715     C                                                    (posA-posDa))
073300030715     C                   LEAVE
073400030715     C                   ELSE
073500030715     C* A questo punto "estraggo" il campo corrente
073600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073700030715     C                                                    (posA-posDa))
073800030715     C                   ENDIF
073900030715     C                   ENDIF
074000030715     C* Incremento il contatore d campo
074100030715     C                   EVAL      i = i +1
074200030715     C                   ENDDO
074300030715     C*
074400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
074500030715     C* delimitazione testo
074600030715     C                   EVAL      i = 1
074700030715     C                   DOW       i < %elem(SkSplitFLD)
074800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
074900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
075000030715     C                   EVAL      i = i + 1
075100030715     C                   ENDDO
075200030715     C                   EVAL      wGiro = 1
075300030715     C                   ELSE
075400030715     C                   EVAL      wGiro = 2
075500031204     C**********
075600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
075700031204     C* d separatore campo
075800031204     C**********
075900031204     C                   EVAL      vindta = %trim(vindta)
076000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
076100031204     C                   EVAL      vindta = %subst(vindta:2)
076200031204     C                   ENDDO
076300031204     C*
076400031204     C                   Z-ADD     *zeros        lunghInput        4 0
076500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
076600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
076700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
076800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
076900031204     C                   ENDDO
077000030715     C**********
077100030715     C* Effettuo lo split del campo dati d input
077200030715     C**********
077300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077400030715     C                   DOW       posDa <= %len(%trim(vindta))
077500030715     C*
077600030715     C* Gestisco il 1° campo
077700030715     C                   IF        i = 1
077800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
077900030715     C                   EVAL      posDa = 1
078000030715     C* Eseguo lo scan x trovare la fine del primo campo
078100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078200030715     C* A questo punto "estraggo" il campo corrente
078300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078400030715     C                                                    (posA-posDa))
078500030715     C* X i campi successivi al 1°
078600030715     C                   ELSE
078700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078800030715     C                   EVAL      posDa = posA + 1
078900031204     C* Verifico che nn vi sia il campo nn valorizzato
079000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
079100031204     C* Se campo nn valorizzato skippo al prossimo
079200031204     C                   EVAL      SkSplitCSV(i) = *blanks
079300031204     C                   EVAL      posA  = posA + 1
079400031204     C                   ELSE
079500030715     C* Eseguo lo scan x trovare la fine del campo corrente
079600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079700030715     C* Gestisco l'ultimo campo
079800030715     C                   IF        posA = *zeros
079900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080000030715     C* A questo punto "estraggo" il campo corrente
080100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080200030715     C                                                    (posA-posDa))
080300030715     C                   LEAVE
080400030715     C                   ELSE
080500030715     C* A questo punto "estraggo" il campo corrente
080600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080700030715     C                                                    (posA-posDa))
080800030715     C                   ENDIF
080900031204     C                   ENDIF
081000030715     C                   ENDIF
081100030715     C* Incremento il contatore d campo
081200030715     C                   EVAL      i = i +1
081300030715     C                   ENDDO
081400030715     C*
081500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081600030715     C* delimitazione testo
081700030715     C                   EVAL      i = 1
081800030715     C                   DOW       i < %elem(SkSplitCSV)
081900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
082000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
082100030715     C                   EVAL      i = i + 1
082200030715     C                   ENDDO
082300030715     C*
082400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
082500030715     C                   EVAL      i = 1
082600030715     C                   DOW       i < %elem(SkSplitFLD)
082700030715     C*
082800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
082900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
083000030715     C                             SkSplitCSV(i) <> *blanks
083100030721     C*** ATB
083200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
083300030721     C                   EVAL      VABATB = SkSplitCSV(i)
083400030721     C                   ENDIF
083500030715     C*** RSD
083600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
083700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
083800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
083900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
084000030715     C* ==
084100030715     C                   ENDIF
084200030721     C*** RD2
084300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
084400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
084500030721     C                   ENDIF
084600030715     C*** IND
084700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
084800030715     C                   EVAL      VABIND = SkSplitCSV(i)
084900030715     C                   ENDIF
085000030715     C*** LOD
085100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
085200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
085300030715     C                   ENDIF
085400030715     C*** PRD
085500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
085600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
085700030715     C                   ENDIF
085800030721     C*** NZD
085900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
086000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
086100030721     C                   ENDIF
086200030715     C*** RMA
086300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
086400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
086500030715     C                   ENDIF
086600030715     C*** NOT
086700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
086800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
086900030715     C                   ENDIF
087000030715     C*** NT2
087100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
087200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
087300030715     C                   ENDIF
087400030715     C*** VCA
087500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
087600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
087700030715     C                   ENDIF
087800030715     C*** VAS
087900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
088000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
088100030715     C                   ENDIF
088200030715     C*** FFD
088300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
088400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
088500030715     C                   ENDIF
088600030721     C*** GC1
088700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
088800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
088900030721     C                   ENDIF
089000030721     C*** GC2
089100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
089200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
089300030721     C                   ENDIF
089400030721     C*** TSP
089500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
089600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
089700030721     C                   ENDIF
089800040510     C*** CBO
089900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
090000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
090100040510     C                   ENDIF
090200030721     C*** NAS
090300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
090400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
090500030721     C                   ENDIF
090600030721     C*** TIC
090700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
090800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
090900030721     C                   ENDIF
091000030721     C*** GCA
091100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
091200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
091300030721     C                   ENDIF
091400030721     C*** XCO
091500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
091600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
091700030721     C                   ENDIF
091800030721     C*** CTM
091900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
092000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
092100030721     C                   ENDIF
092200030721     C*** TCR
092300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
092400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
092500030721     C                   ENDIF
092600030721     C*** CTS
092700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
092800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
092900030721     C                   ENDIF
093000030721     C*** FTM
093100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
093200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
093300030721     C                   ENDIF
093400030721     C*** VAD
093500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
093600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
093700030721     C                   ENDIF
093800030721     C*** GMA
093900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
094000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
094100030721     C                   ENDIF
094200030721     C*** GGA
094300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
094400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
094500030721     C                   ENDIF
094600030721     C*** GVA
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
094800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
094900030721     C                   ENDIF
095000030721     C*** TC1
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
095200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
095300030721     C                   ENDIF
095400030721     C*** TC2
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
095600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030721     C*** SCL
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
096000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
096100030721     C                   ENDIF
096200030721     C*** RMO
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
096400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
096500030721     C                   ENDIF
096600030721     C*** NMO
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
096800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030721     C*
097100030721     C* Reperisco quindi i campi numerici...
097200040510     C*** NSP
097300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
097400040510     C                   EVAL      PiStr=SkSplitCSV(i)
097500040510     C                   EXSR      CHKNUM
097600040510     C                   IF        PiInt=*on
097700040510     C                   Z-ADD     PiVal         VABNSP
097800040510     C                   ELSE
097900040510     C                   SETON                                        31
098000040510     C                   EVAL      VABNSP = *zeros
098100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
098200040510     C                             + ' ' + 'VABNSP'
098300040510     C                   ENDIF
098400040510     C                   ENDIF
098500030721     C*** CAD
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
098700030721     C                   EVAL      PiStr=SkSplitCSV(i)
098800030721     C                   EXSR      CHKNUM
098900030721     C                   IF        PiInt=*on
099000030721     C                   Z-ADD     PiVal         Num5_0
099100030721     C                   MOVEL(p)  Num5_0        VABCAD
099200030721     C                   ELSE
099300030721     C                   SETON                                        32
099400030721     C                   EVAL      VABCAD = *zeros
099500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099600030721     C                             + ' ' + 'VABCAD'
099700030721     C                   ENDIF
099800030721     C                   ENDIF
099900030721     C*** NCL
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
100100030721     C                   EVAL      PiStr=SkSplitCSV(i)
100200030721     C                   EXSR      CHKNUM
100300030721     C                   IF        PiInt=*on
100400030721     C                   Z-ADD     PiVal         VABNCL
100500030721     C                   ELSE
100600030721     C                   SETON                                        32
100700030721     C                   EVAL      VABNCL = *zeros
100800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100900040122     C                             + ' ' + 'VABNCL'
101000030721     C                   ENDIF
101100030721     C                   ENDIF
101200030721     C*** PKB
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
101400030721     C                   EVAL      PiStr=SkSplitCSV(i)
101500030721     C                   EXSR      CHKNUM
101600030721     C                   IF        PiNum=*on
101700030721     C                   Z-ADD     PiVal         VABPKB
101800030721     C                   ELSE
101900030721     C                   SETON                                        32
102000030721     C                   EVAL      VABPKB = *zeros
102100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102200030721     C                             + ' ' + 'VABPKB'
102300030721     C                   ENDIF
102400030721     C                   ENDIF
102500030721     C*** CAS
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
102700030721     C                   EVAL      PiStr=SkSplitCSV(i)
102800030721     C                   EXSR      CHKNUM
102900030721     C                   IF        PiNum=*on
103000030721     C                   Z-ADD     PiVal         VABCAS
103100030721     C                   ELSE
103200030721     C                   SETON                                        32
103300030721     C                   EVAL      VABCAS = *zeros
103400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103500030721     C                             + ' ' + 'VABCAS'
103600030721     C                   ENDIF
103700030721     C                   ENDIF
103800030721     C*** CCM
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
104000030721     C                   EVAL      PiStr=SkSplitCSV(i)
104100030721     C                   EXSR      CHKNUM
104200030721     C                   IF        PiInt=*on      AND
104300030721     C                             PiVal<=9999999 AND
104400030721     C                             PiVal>*zeros
104500030721     C                   Z-ADD     PiVal         VABCCM
104600030721     C                   ELSE
104700030721     C                   SETON                                        32
104800030721     C                   EVAL      VABCCM = *zeros
104900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105000040510     C                             + ' ' + 'VABCCM'
105100030721     C                   ENDIF
105200030721     C                   ENDIF
105300030721     C*** LNP
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
105500030721     C                   EVAL      PiStr=SkSplitCSV(i)
105600030721     C                   EXSR      CHKNUM
105700030721     C                   IF        PiInt=*on    AND
105800030721     C                             PiVal<=999   AND
105900030721     C                             PiVal>*zeros
106000030721     C                   Z-ADD     PiVal         VABLNP
106100030721     C                   ELSE
106200040715     C                   SETON                                        32
106300030721     C                   EVAL      VABLNP = *zeros
106400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106500040510     C                             + ' ' + 'VABLNP'
106600030721     C                   ENDIF
106700030721     C                   ENDIF
106800030721     C*** NRS
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
107000030721     C                   EVAL      PiStr=SkSplitCSV(i)
107100030721     C                   EXSR      CHKNUM
107200030721     C                   IF        PiInt=*on AND
107300030721     C                             PiVal<=99
107400030721     C                   Z-ADD     PiVal         VABNRS
107500030721     C                   ELSE
107600030721     C                   SETON                                        32
107700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107800030721     C                             + ' ' + 'VABNRS'
107900030721     C                   ENDIF
108000030721     C                   ENDIF
108100030721     C*** LNA
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
108300030721     C                   EVAL      PiStr=SkSplitCSV(i)
108400030721     C                   EXSR      CHKNUM
108500030721     C                   IF        PiInt=*on    AND
108600030721     C                             PiVal<=999   AND
108700030721     C                             PiVal>*zeros
108800030721     C                   Z-ADD     PiVal         VABLNA
108900030721     C                   ELSE
109000030721     C                   SETON                                        32
109100030721     C                   EVAL      VABLNA = *zeros
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300030721     C                             + ' ' + 'VABLNA'
109400030721     C                   ENDIF
109500030721     C                   ENDIF
109600030721     C*** CTR
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
109800030721     C                   EVAL      PiStr=SkSplitCSV(i)
109900030721     C                   EXSR      CHKNUM
110000030721     C                   IF        PiInt=*on  AND
110100030721     C                             PiVal<=999
110200030721     C                   Z-ADD     PiVal         VABCTR
110300030721     C                   ELSE
110400030721     C                   SETON                                        32
110500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110600030721     C                             + ' ' + 'VABCTR'
110700030721     C                   ENDIF
110800030721     C                   ENDIF
110900030721     C*** IAS
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
111100030721     C                   EVAL      PiStr=SkSplitCSV(i)
111200030721     C                   EXSR      CHKNUM
111300030721     C                   IF        PiNum=*on
111400030721     C                   Z-ADD     PiVal         VABIAS
111500030721     C                   ELSE
111600030721     C                   SETON                                        32
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800030721     C                             + ' ' + 'VABIAS'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** VLB
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiNum=*on
112600030721     C                   Z-ADD     PiVal         VABVLB
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113000030721     C                             + ' ' + 'VABVLB'
113100030721     C                   ENDIF
113200030721     C                   ENDIF
113300030721     C*** QFT
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
113500030721     C                   EVAL      PiStr=SkSplitCSV(i)
113600030721     C                   EXSR      CHKNUM
113700030721     C                   IF        PiNum=*on
113800030721     C                   Z-ADD     PiVal         VABQFT
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200030721     C                             + ' ' + 'VABQFT'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500030721     C*** NCD
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiInt=*on      AND
115000030721     C                             PiVal<=9999999
115100030721     C                   Z-ADD     PiVal         VABNCD
115200030721     C                   ELSE
115300030721     C                   SETON                                        32
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500030721     C                             + ' ' + 'VABNCD'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** NCA
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiInt=*on      AND
116300030721     C                             PiVal<=9999999
116400030721     C                   Z-ADD     PiVal         VABNCA
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800030721     C                             + ' ' + 'VABNCA'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** ZNC
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on AND
117600030721     C                             PiVal<=99
117700030721     C                   Z-ADD     PiVal         VABZNC
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABZNC'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** DCR
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiInt=*on       AND
118900030721     C                             PiVal<=99999999
119000030721     C                   Z-ADD     PiVal         VABDCR
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400030721     C                             + ' ' + 'VABDCR'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** HCR
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiInt=*on   AND
120200030721     C                             PiVal<=9999
120300030721     C                   Z-ADD     PiVal         VABHCR
120400030721     C                   ELSE
120500030721     C                   SETON                                        32
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABHCR'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** VMD
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiNum=*on
121500030721     C                   Z-ADD     PiVal         VABVMD
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABVMD'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** ANT
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiInt=*on        AND
122700030721     C                             PiVal<=999999999
122800030721     C                   Z-ADD     PiVal         VABANT
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABANT'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** CMO
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on
124000030721     C                   Z-ADD     PiVal         Num5_0
124100030721     C                   MOVEL(p)  Num5_0        VABCMO
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      VABCMO = *zeros
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600030721     C                             + ' ' + 'VABCMO'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900040510     C*
125000040510     C* Reperisco i campi del FIVAT
125100040510     C*** VATNOT
125200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
125300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
125400040510     C                   ENDIF
125500040510     C*** VATTRC
125600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
125700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
125800040510     C                   ENDIF
125900060331     C*** VATNOT_A
126000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
126100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
126200060331     C                   ENDIF
126300060331     C*** VATNOT_B
126400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
126500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
126600060331     C                   ENDIF
126700060331     C*** VATNOT_E
126800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
126900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
127000060331     C                   ENDIF
127100070823     C*
127200070907     C***  ===>  Gestione campi "particolari"
127300070910     C                   EVAL      VABQFT = VABVLB
127400070907     C***  <===  ----------------------------
127500040510     C*
127600030715     C                   ENDIF
127700030715     C                   EVAL      i = i + 1
127800030715     C                   ENDDO
127900040510     C*
128000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
128100040510     C                   IF        VABNSP = *zeros
128200040510     C                   SETON                                        31
128300040510     C                   ENDIF
128400040715     C*
128500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
128600040715     C                   IF        VABLNP = *zeros
128700040715     C                   SETON                                        31
128800040715     C                   ENDIF
128900040510     C*
129000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
129100040510     C                   IF        VABRMN = *zeros
129200040510     C                   Z-ADD     VABNSP        VABRMN
129300040510     C                   ENDIF
129400040510     C*
129500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
129600040510     C                   IF        VABRMA = *blanks
129700040714     C                   MOVEL     VABRMN        VABRMA
129800040510     C                   ENDIF
129900040128     C*
130000040510     C* Se provincia nn valorizzata la reperisco
130100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
130200040510     C                   IF        VABPRD = *blanks
130300040128     C                   CLEAR                   TISI95DS
130400040128     C                   EVAL      I95TCN = '3'
130500040128     C                   Z-ADD     datcor        I95DAT
130600040128     C                   EVAL      I95NAR = VABNZD
130700040128     C                   EVAL      I95CAP = VABCAD
130800040128     C                   EVAL      I95LOC = VABLOD
130900040128     C                   CALL      'TISI95R'
131000040128     C                   PARM                    TISI95DS
131100040128     C                   EVAL      VABPRD = O95PRV
131200040510     C                   ENDIF
131300040113     C*
131400040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
131500040113     C                   IF        VABCAS > *zeros
131600040119     C                   EVAL      VABVCA = 'EUR'
131700040119     C                   IF        VABCBO = '1'
131800040113     C                   EVAL      VABCBO = '4'
131900040119     C                   ENDIF
132000040119     C                   IF        VABCBO = '2'
132100040119     C                   EVAL      VABCBO = '6'
132200040119     C                   ENDIF
132300040113     C                   ENDIF
132400020204     C*
132500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
132600020204     C                   EXSR      CHKIMPDIV
132700030715     C                   ENDIF
132800020204     C*
132900010607     C                   ENDSR
133000010607     C*----------------------------------------------------*
133100010601
133200020204
133300020204     C*----------------------------------------------------*
133400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
133500020204     C*----------------------------------------------------*
133600020204     C     CHKIMPDIV     BEGSR
133700020204     C*
133800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
133900020204     C                   Z-ADD     *zeros        wrkDec            9 9
134000020204     C*
134100020204     C* Come prima cosa effettuo considerazioni sulla divisa
134200020204     C                   IF        vabIAS > *zeros
134300020204     C                   IF        vabVAS <> 'EUR'
134400020204     C                   EVAL      vabVAS =  'ITL'
134500020204     C                   ENDIF
134600020204     C                   ENDIF
134700020204     C*
134800020204     C                   IF        vabCAS > *zeros
134900020204     C                   IF        vabVCA <> 'EUR'
135000020204     C                   EVAL      vabVCA =  'ITL'
135100020204     C                   ENDIF
135200020204     C                   ENDIF
135300020204     C*
135400020204     C                   IF        vabVMD > *zeros
135500020204     C                   IF        vabVAD <> 'EUR'
135600020204     C                   EVAL      vabVAD =  'ITL'
135700020204     C                   ENDIF
135800020204     C                   ENDIF
135900020204     C*
136000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
136100020204     C                   Z-ADD     vabIAS        wrkDec
136200020204     C                   IF        wrkDec > *zeros
136300020204     C                   IF        vabVAS = 'ITL'
136400020204     C                   EVAL      vabIAS = *zeros
136500020204     C                   ENDIF
136600020204     C                   ENDIF
136700020204     C*
136800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
136900020204     C                   Z-ADD     vabCAS        wrkDec
137000020204     C                   IF        wrkDec > *zeros
137100020204     C                   IF        vabVCA = 'ITL'
137200020204     C                   EVAL      vabCAS = *zeros
137300020204     C                   ENDIF
137400020204     C                   ENDIF
137500020204     C*
137600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
137700020204     C                   Z-ADD     vabVMD        wrkDec
137800020204     C                   IF        wrkDec > *zeros
137900020204     C                   IF        vabVAD = 'ITL'
138000020204     C                   EVAL      vabVMD = *zeros
138100020204     C                   ENDIF
138200020204     C                   ENDIF
138300020204     C*
138400020204     C                   ENDSR
138500020204     C***
138600020204
138700010330
138800010330
138900010330     C*----------------------------------------------------*
139000010330     C*  CONTROLLO NUMERICITA' CAMPI
139100010330     C*----------------------------------------------------*
139200010330     C     CHKNUM        BEGSR
139300010330     C*
139400010606     C                   IF        PiDecChr = *blanks
139500030715     C                   EVAL      PiDecChr = CharNUM
139600010606     C                   ENDIF
139700010606     C*
139800010606     C                   CALL(e)   'ISNUMERIC'
139900010330     C                   PARM                    PiStr            30
140000010606     C                   PARM                    PiDecChr          1
140100010330     C                   PARM      *ZEROS        PiVal            30 9
140200010330     C                   PARM      '0'           PiInt             1
140300010330     C                   PARM      '0'           PiNum             1
140400010330     C                   IF        %error
140500010606     C                   EVAL      PiNum=*off
140600010330     C                   ENDIF
140700010330     C*
140800010330     C                   ENDSR
140900010330     C***
141000010330
141100010601
141200010601
141300010601
141400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
141500010601     C     repfil        BEGSR
141600010601     C*
141700010601     C                   if        invfil = *zeros and
141800010601     C                             depfil > *zeros and
141900010629     C                             (vinflg = *blanks or
142000010629     C                              vinflg = *zeros)
142100010601     C
142200010601     C                   eval      invfil = depfil
142300010601     C                   endif
142400010601     C*
142500010601     C                   if        depfil <> invfil and
142600010601     C                             invfil > *zeros
142700010601     C                   eval      flgMulti = '1'
142800010601     C                   if        vinflg = *blanks
142900010601     C                   add       1             cntNonEl
143000010601     C                   endif
143100010601     C                   endif
143200010601     C*
143300010601     C                   if        vinflg = '2'
143400010601     C                   eval      flgStato = '2'
143500010601     C                   endif
143600010601     C*
143700010601     C                   ENDSR
143800010601     C***
143900010601
144000010601
144100010601
144200010330
144300040119      /TITLE Invio dei dati al punto operativo.
144400040119     C     invio         BEGSR
144500040119     C*
144600040119     C* 1° invio FIVAT
144700040119     C                   reset                   dscmz
144800040119     C                   move      vlrpoi        cmzdst
144900040119     C                   eval      cmzfld = 'FIVATWWR'
145000040119     C                   eval      cmzmbd = vlrhdl
145100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
145200040119     C***                if        prmfir = *blanks
145300040119     C                   eval      cmzfla = 'FIVAT00F'
145400040119     C                   eval      cmzmba = 'FIVAT00F'
145500040119     C***                else
145600040119     C***                eval      cmzfla = prmfir
145700040119     C***                eval      cmzmba = prmfir
145800040119     C***                endif
145900040119     C                   eval      cmznrr = *zeros
146000040119     C                   move      §ctrokvt      cmznrr
146100040119     C                   eval      cmzlba = vlrfl1
146200040119     C                   call(e)   'TIS711C'
146300040119     C                   parm                    dscmz
146400040119     C                   parm      *blanks       esito
146500040119     C                   if        %error
146600040119     C                             or cmzerr = '1'
146700040119     C                             or esito  = '1'
146800040119     C                   eval      wrkesito = '3'
146900040119     C                   else
147000040119     C*
147100040119     C* 2° invio FIVAB
147200040119     C                   reset                   dscmz
147300040119     C                   move      vlrpoi        cmzdst
147400040119     C                   eval      cmzfld = vlrfou
147500040119     C                   eval      cmzmbd = vlrhdl
147600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
147700040119     C***                if        prmfir = *blanks
147800040119     C                   eval      cmzfla = 'FIVAB00F'
147900040119     C                   eval      cmzmba = 'FIVAB00F'
148000040119     C***                else
148100040119     C***                eval      cmzfla = prmfir
148200040119     C***                eval      cmzmba = prmfir
148300040119     C***                endif
148400040119     C                   eval      cmznrr = *zeros
148500040119     C                   move      §ctrokvb      cmznrr
148600040119     C                   eval      cmzlba = vlrfl1
148700040119     C                   call(e)   'TIS711C'
148800040119     C                   parm                    dscmz
148900040119     C                   parm      *blanks       esito
149000040119     C                   if        %error
149100040119     C                             or cmzerr = '1'
149200040119     C                             or esito  = '1'
149300040119     C                   eval      wrkesito = '3'
149400040119     C                   endif
149500040119     C                   endif
149600040119     C*
149700040119     C                   ENDSR
149800040119     C***
149900010601
150000010601
150100010601
150200010601
150300010601      /TITLE Invio dei dati al punto operativo.
150400010601     C     opeini        BEGSR
150500010601     C*
150600010601     C* Inizializzo flag e contatori operativi
150700010601     C                   movel     '0'           flgGiro           1
150800010601     C                   movel     '0'           flgMulti          1
150900010601     C                   movel     '1'           flgStato          1
151000010615     C                   movel     '0'           flgOk             1
151100010601     C                   z-add     *zeros        cntNonEl         10 0
151200010601     C                   z-add     *zeros        depfil            3 0
151300010601     C                   z-add     *zeros        invfil            3 0
151400010601     C*
151500010601     C                   ENDSR
151600010601     C***
151700070326
151800070326
151900070326
152000070326
152100070326     C     *pssr         BEGSR
152200070329     C*
152300070329     C                   if        %open(tivin00r)
152400070329     C                   close     tivin00r
152500070329     C                   endif
152600070329     C                   if        %open(fivabwwr)
152700070329     C                   close     fivabwwr
152800070329     C                   endif
152900070329     C                   if        %open(fivatwwr)
153000070329     C                   close     fivatwwr
153100070329     C                   endif
153200070326     C*
153300070326     C* Effettuo la chiamata al CLLE preposto
153400070326     C                   call(e)   'TITVVTC'
153500070326     C                   parm                    parccm
153600070326     C                   parm                    parmbr
153700070326     C                   parm      '2'           paropz
153800070326     C*
153900070326     C                   eval      wrkesito = '2'
154000070404     C*
154100070404     C                   seton                                        LR
154200070326     C*
154300070326     C                   ENDSR     '*CANCL'
154400070326     C***
154500070326
154600070326
154700010330
154800010330
154900000613     C     *inzsr        BEGSR
155000990910     C*
155100990910     C     *entry        plist
155200990920     C                   parm                    tivlrds
155300990921     C                   parm      wrkesito      esito
155400000724     C                   parm                    prmlit
155500000710     C                   parm                    prmfir
155600010330     C*
155700010330     C* CALCOLA LA DATA CORRENTE
155800010330     C                   time                    wn14             14 0
155900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
156000010330     C                   z-add     wn8           g08dat
156100010330     C                   z-add     *zeros        g08inv
156200010330     C                   movel     '0'           g08err
156300010330     C                   call      'XSRDA8'
156400010330     C                   parm                    wlbda8
156500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
156600000613     C*
156700000613     C                   ENDSR
156800000613     C***
156900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
157000070907FILL1;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;VABVLB;
157100070907VABCAS;VABTIC;VABNSP;VATNOT_E;VABNOT
157200070907
