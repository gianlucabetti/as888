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
011500000613     C*
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
043000030822     C*
043100030822     C* Gestisco il 1° giro
043200030822     C                   IF        wGiroDft = 0
043300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
043400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
043500030822     C* Incremento il contatore dei "giri"
043600030822     C                   EVAL      wGiroDft = 1
043700030822     C                   ELSE
043800030822     C                   EVAL      posDaDft = posADft
043900030822     C                   ENDIF
044000030822     C* Eseguo lo scan x trovare la fine del campo corrente
044100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
044200030822     C*
044300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
044400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
044500030822     C                   IF        posDaDft > 0 AND
044600030822     C                             posADft  > 0
044700030822     C* NCL
044800030822     C                   IF        %subst(
044900030822     C                             %subst(vlrppt:posDaDft+1:
045000030822     C                             posADft-posDaDft-1):1:3)
045100030822     C                             = 'NCL'
045200030822     C                   EVAL      PiStr=%trim(%subst(
045300030822     C                             %subst(vlrppt:posDaDft+1:
045400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045500030822     C                   EXSR      CHKNUM
045600030822     C                   IF        PiInt=*on
045700030822     C                   Z-ADD     PiVal         VABNCL
045800030822     C                   ENDIF
045900030822     C                   ENDIF
046000030822     C* CCM
046100030822     C                   IF        %subst(
046200030822     C                             %subst(vlrppt:posDaDft+1:
046300030822     C                             posADft-posDaDft-1):1:3)
046400030822     C                             = 'CCM'
046500030822     C                   EVAL      PiStr=%trim(%subst(
046600030822     C                             %subst(vlrppt:posDaDft+1:
046700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046800030822     C                   EXSR      CHKNUM
046900030822     C                   IF        PiInt=*on
047000030822     C                   Z-ADD     PiVal         VABCCM
047100030822     C                   ENDIF
047200030822     C                   ENDIF
047300030822     C* LNP
047400030822     C                   IF        %subst(
047500030822     C                             %subst(vlrppt:posDaDft+1:
047600030822     C                             posADft-posDaDft-1):1:3)
047700030822     C                             = 'LNP'
047800030822     C                   EVAL      PiStr=%trim(%subst(
047900030822     C                             %subst(vlrppt:posDaDft+1:
048000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048100030822     C                   EXSR      CHKNUM
048200030822     C                   IF        PiInt=*on
048300030822     C                   Z-ADD     PiVal         VABLNP
048400030822     C                   ENDIF
048500030822     C                   ENDIF
048600030822     C* NRS
048700030822     C                   IF        %subst(
048800030822     C                             %subst(vlrppt:posDaDft+1:
048900030822     C                             posADft-posDaDft-1):1:3)
049000030822     C                             = 'NRS'
049100030822     C                   EVAL      PiStr=%trim(%subst(
049200030822     C                             %subst(vlrppt:posDaDft+1:
049300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049400030822     C                   EXSR      CHKNUM
049500030822     C                   IF        PiInt=*on
049600030822     C                   Z-ADD     PiVal         VABNRS
049700030822     C                   ENDIF
049800030822     C                   ENDIF
049900030822     C* CTR
050000030822     C                   IF        %subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):1:3)
050300030822     C                             = 'CTR'
050400030822     C                   EVAL      PiStr=%trim(%subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050700030822     C                   EXSR      CHKNUM
050800030822     C                   IF        PiInt=*on
050900030822     C                   Z-ADD     PiVal         VABCTR
051000030822     C                   ENDIF
051100030822     C                   ENDIF
051200030822     C* PKB
051300030822     C                   IF        %subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):1:3)
051600030822     C                             = 'PKB'
051700030822     C                   EVAL      PiStr=%trim(%subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052000030822     C                   EXSR      CHKNUM
052100030822     C                   IF        PiNum=*on
052200030822     C                   Z-ADD     PiVal         VABPKB
052300030822     C                   ENDIF
052400030822     C                   ENDIF
052500030822     C* VLB
052600030822     C                   IF        %subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):1:3)
052900030822     C                             = 'VLB'
053000030822     C                   EVAL      PiStr=%trim(%subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300030822     C                   EXSR      CHKNUM
053400030822     C                   IF        PiNum=*on
053500030822     C                   Z-ADD     PiVal         VABVLB
053600030822     C                   ENDIF
053700030822     C                   ENDIF
053800030822     C* QFT
053900030822     C                   IF        %subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):1:3)
054200030822     C                             = 'QFT'
054300030822     C                   EVAL      PiStr=%trim(%subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600030822     C                   EXSR      CHKNUM
054700030822     C                   IF        PiNum=*on
054800030822     C                   Z-ADD     PiVal         VABQFT
054900030822     C                   ENDIF
055000030822     C                   ENDIF
055100030822     C* CBO
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'CBO'
055600030822     C                   EVAL      VABCBO=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   ENDIF
056000030822     C* TSP
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'TSP'
056500030822     C                   EVAL      VABTSP=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   ENDIF
056900030822     C* VAS
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'VAS'
057400030822     C                   EVAL      VABVAS=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   ENDIF
057800030822     C* VCA
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'VCA'
058300030822     C                   EVAL      VABVCA=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   ENDIF
058700030822     C* GCA
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'GCA'
059200030822     C                   EVAL      VABGCA=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   ENDIF
059600030822     C* CTM
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'CTM'
060100030822     C                   EVAL      VABCTM=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   ENDIF
060500030822     C* FFD
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900030822     C                             = 'FFD'
061000030822     C                   EVAL      VABFFD=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   ENDIF
061400030822     C* VAD
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'VAD'
061900030822     C                   EVAL      VABVAD=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   ENDIF
062300030822     C* GMA
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'GMA'
062800030822     C                   EVAL      VABGMA=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   ENDIF
063200030822     C* GGA
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'GGA'
063700030822     C                   EVAL      VABGGA=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   ENDIF
064100030822     C* GVA
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'GVA'
064600030822     C                   EVAL      VABGVA=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   ENDIF
065000030822     C* TC1
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'TC1'
065500030822     C                   EVAL      VABTC1=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* TC2
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'TC2'
066400030822     C                   EVAL      VABTC2=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800040714     C* VATTRC
066900040714     C                   IF        %subst(
067000040714     C                             %subst(vlrppt:posDaDft+1:
067100040714     C                             posADft-posDaDft-1):1:3)
067200040714     C                             = 'TRC'
067300040714     C                   EVAL      VATTRC=%trim(%subst(
067400040714     C                             %subst(vlrppt:posDaDft+1:
067500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600040714     C                   ENDIF
067700030822     C* ...
067800030822     C                   ENDIF
067900030822     C                   ENDDO
068000020204     C*
068100010330     C                   ENDSR
068200010607     C*----------------------------------------------------*
068300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
068400010607     C*----------------------------------------------------*
068500010607     C     IMPVAB        BEGSR
068600010607     C*
068700010607     C                   SETOFF                                       3132
068800030515     C*
068900010607     C                   MOVEL     datcor        VABAAS
069000010607     C                   MOVE      datcor        VABMGS
069100010607     C*
069200030715     C**********
069300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
069400030715     C**********
069500030715     C                   IF        wGiro = *zeros
069600040113     C*
069700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
069800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
069900040510     C                             %trim(intcol(3))<>*blanks
070000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
070100040113     C                                      %trim(intcol(3))
070200070823     C     ';':CharCSV   XLATE     vindta        vindta
070300040510     C                   ENDIF
070400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
070500031201     C     minu:maiu     XLATE     vindta        vindta
070600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
070700030715     C                   DOW       posDa <= %len(%trim(vindta))
070800030715     C*
070900030715     C* Gestisco il 1° campo
071000030715     C                   IF        i = 1
071100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
071200030715     C                   EVAL      posDa = 1
071300030715     C* Eseguo lo scan x trovare la fine del primo campo
071400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071500030715     C* A questo punto "estraggo" il campo corrente
071600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
071700030715     C                                                    (posA-posDa))
071800030715     C* X i campi successivi al 1°
071900030715     C                   ELSE
072000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
072100030715     C                   EVAL      posDa = posA + 1
072200030715     C* Eseguo lo scan x trovare la fine del campo corrente
072300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072400030715     C* Gestisco l'ultimo campo
072500030715     C                   IF        posA = *zeros
072600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
072700030715     C* A questo punto "estraggo" il campo corrente
072800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
072900030715     C                                                    (posA-posDa))
073000030715     C                   LEAVE
073100030715     C                   ELSE
073200030715     C* A questo punto "estraggo" il campo corrente
073300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073400030715     C                                                    (posA-posDa))
073500030715     C                   ENDIF
073600030715     C                   ENDIF
073700030715     C* Incremento il contatore d campo
073800030715     C                   EVAL      i = i +1
073900030715     C                   ENDDO
074000030715     C*
074100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
074200030715     C* delimitazione testo
074300030715     C                   EVAL      i = 1
074400030715     C                   DOW       i < %elem(SkSplitFLD)
074500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
074600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
074700030715     C                   EVAL      i = i + 1
074800030715     C                   ENDDO
074900030715     C                   EVAL      wGiro = 1
075000030715     C                   ELSE
075100030715     C                   EVAL      wGiro = 2
075200031204     C**********
075300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
075400031204     C* d separatore campo
075500031204     C**********
075600031204     C                   EVAL      vindta = %trim(vindta)
075700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
075800031204     C                   EVAL      vindta = %subst(vindta:2)
075900031204     C                   ENDDO
076000031204     C*
076100031204     C                   Z-ADD     *zeros        lunghInput        4 0
076200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
076300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
076400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
076500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
076600031204     C                   ENDDO
076700030715     C**********
076800030715     C* Effettuo lo split del campo dati d input
076900030715     C**********
077000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077100030715     C                   DOW       posDa <= %len(%trim(vindta))
077200030715     C*
077300030715     C* Gestisco il 1° campo
077400030715     C                   IF        i = 1
077500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
077600030715     C                   EVAL      posDa = 1
077700030715     C* Eseguo lo scan x trovare la fine del primo campo
077800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077900030715     C* A questo punto "estraggo" il campo corrente
078000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078100030715     C                                                    (posA-posDa))
078200030715     C* X i campi successivi al 1°
078300030715     C                   ELSE
078400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078500030715     C                   EVAL      posDa = posA + 1
078600031204     C* Verifico che nn vi sia il campo nn valorizzato
078700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
078800031204     C* Se campo nn valorizzato skippo al prossimo
078900031204     C                   EVAL      SkSplitCSV(i) = *blanks
079000031204     C                   EVAL      posA  = posA + 1
079100031204     C                   ELSE
079200030715     C* Eseguo lo scan x trovare la fine del campo corrente
079300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079400030715     C* Gestisco l'ultimo campo
079500030715     C                   IF        posA = *zeros
079600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
079700030715     C* A questo punto "estraggo" il campo corrente
079800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079900030715     C                                                    (posA-posDa))
080000030715     C                   LEAVE
080100030715     C                   ELSE
080200030715     C* A questo punto "estraggo" il campo corrente
080300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080400030715     C                                                    (posA-posDa))
080500030715     C                   ENDIF
080600031204     C                   ENDIF
080700030715     C                   ENDIF
080800030715     C* Incremento il contatore d campo
080900030715     C                   EVAL      i = i +1
081000030715     C                   ENDDO
081100030715     C*
081200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081300030715     C* delimitazione testo
081400030715     C                   EVAL      i = 1
081500030715     C                   DOW       i < %elem(SkSplitCSV)
081600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
081700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
081800030715     C                   EVAL      i = i + 1
081900030715     C                   ENDDO
082000030715     C*
082100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
082200030715     C                   EVAL      i = 1
082300030715     C                   DOW       i < %elem(SkSplitFLD)
082400030715     C*
082500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
082600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
082700030715     C                             SkSplitCSV(i) <> *blanks
082800030721     C*** ATB
082900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
083000030721     C                   EVAL      VABATB = SkSplitCSV(i)
083100030721     C                   ENDIF
083200030715     C*** RSD
083300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
083400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
083500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
083600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
083700030715     C* ==
083800030715     C                   ENDIF
083900030721     C*** RD2
084000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
084100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
084200030721     C                   ENDIF
084300030715     C*** IND
084400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
084500030715     C                   EVAL      VABIND = SkSplitCSV(i)
084600030715     C                   ENDIF
084700030715     C*** LOD
084800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
084900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
085000030715     C                   ENDIF
085100030715     C*** PRD
085200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
085300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
085400030715     C                   ENDIF
085500030721     C*** NZD
085600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
085700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
085800030721     C                   ENDIF
085900030715     C*** RMA
086000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
086100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
086200030715     C                   ENDIF
086300030715     C*** NOT
086400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
086500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
086600030715     C                   ENDIF
086700030715     C*** NT2
086800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
086900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
087000030715     C                   ENDIF
087100030715     C*** VCA
087200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
087300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
087400030715     C                   ENDIF
087500030715     C*** VAS
087600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
087700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
087800030715     C                   ENDIF
087900030715     C*** FFD
088000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
088100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
088200030715     C                   ENDIF
088300030721     C*** GC1
088400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
088500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
088600030721     C                   ENDIF
088700030721     C*** GC2
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
088900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
089000030721     C                   ENDIF
089100030721     C*** TSP
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
089300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
089400030721     C                   ENDIF
089500040510     C*** CBO
089600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
089700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
089800040510     C                   ENDIF
089900030721     C*** NAS
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
090100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** TIC
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
090500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** GCA
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
090900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** XCO
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
091300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030721     C*** CTM
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
091700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030721     C*** TCR
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
092100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** CTS
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
092500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030721     C*** FTM
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
092900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** VAD
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
093300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** GMA
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
093700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*** GGA
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
094100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030721     C*** GVA
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
094500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*** TC1
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
094900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*** TC2
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
095300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030721     C*** SCL
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
095700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030721     C*** RMO
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
096100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** NMO
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
096500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*
096800030721     C* Reperisco quindi i campi numerici...
096900040510     C*** NSP
097000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
097100040510     C                   EVAL      PiStr=SkSplitCSV(i)
097200040510     C                   EXSR      CHKNUM
097300040510     C                   IF        PiInt=*on
097400040510     C                   Z-ADD     PiVal         VABNSP
097500040510     C                   ELSE
097600040510     C                   SETON                                        31
097700040510     C                   EVAL      VABNSP = *zeros
097800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
097900040510     C                             + ' ' + 'VABNSP'
098000040510     C                   ENDIF
098100040510     C                   ENDIF
098200030721     C*** CAD
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
098400030721     C                   EVAL      PiStr=SkSplitCSV(i)
098500030721     C                   EXSR      CHKNUM
098600030721     C                   IF        PiInt=*on
098700030721     C                   Z-ADD     PiVal         Num5_0
098800030721     C                   MOVEL(p)  Num5_0        VABCAD
098900030721     C                   ELSE
099000030721     C                   SETON                                        32
099100030721     C                   EVAL      VABCAD = *zeros
099200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099300030721     C                             + ' ' + 'VABCAD'
099400030721     C                   ENDIF
099500030721     C                   ENDIF
099600030721     C*** NCL
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
099800030721     C                   EVAL      PiStr=SkSplitCSV(i)
099900030721     C                   EXSR      CHKNUM
100000030721     C                   IF        PiInt=*on
100100030721     C                   Z-ADD     PiVal         VABNCL
100200030721     C                   ELSE
100300030721     C                   SETON                                        32
100400030721     C                   EVAL      VABNCL = *zeros
100500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100600040122     C                             + ' ' + 'VABNCL'
100700030721     C                   ENDIF
100800030721     C                   ENDIF
100900030721     C*** PKB
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
101100030721     C                   EVAL      PiStr=SkSplitCSV(i)
101200030721     C                   EXSR      CHKNUM
101300030721     C                   IF        PiNum=*on
101400030721     C                   Z-ADD     PiVal         VABPKB
101500030721     C                   ELSE
101600030721     C                   SETON                                        32
101700030721     C                   EVAL      VABPKB = *zeros
101800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101900030721     C                             + ' ' + 'VABPKB'
102000030721     C                   ENDIF
102100030721     C                   ENDIF
102200030721     C*** CAS
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
102400030721     C                   EVAL      PiStr=SkSplitCSV(i)
102500030721     C                   EXSR      CHKNUM
102600030721     C                   IF        PiNum=*on
102700030721     C                   Z-ADD     PiVal         VABCAS
102800030721     C                   ELSE
102900030721     C                   SETON                                        32
103000030721     C                   EVAL      VABCAS = *zeros
103100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103200030721     C                             + ' ' + 'VABCAS'
103300030721     C                   ENDIF
103400030721     C                   ENDIF
103500030721     C*** CCM
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
103700030721     C                   EVAL      PiStr=SkSplitCSV(i)
103800030721     C                   EXSR      CHKNUM
103900030721     C                   IF        PiInt=*on      AND
104000030721     C                             PiVal<=9999999 AND
104100030721     C                             PiVal>*zeros
104200030721     C                   Z-ADD     PiVal         VABCCM
104300030721     C                   ELSE
104400030721     C                   SETON                                        32
104500030721     C                   EVAL      VABCCM = *zeros
104600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104700040510     C                             + ' ' + 'VABCCM'
104800030721     C                   ENDIF
104900030721     C                   ENDIF
105000030721     C*** LNP
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
105200030721     C                   EVAL      PiStr=SkSplitCSV(i)
105300030721     C                   EXSR      CHKNUM
105400030721     C                   IF        PiInt=*on    AND
105500030721     C                             PiVal<=999   AND
105600030721     C                             PiVal>*zeros
105700030721     C                   Z-ADD     PiVal         VABLNP
105800030721     C                   ELSE
105900040715     C                   SETON                                        32
106000030721     C                   EVAL      VABLNP = *zeros
106100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106200040510     C                             + ' ' + 'VABLNP'
106300030721     C                   ENDIF
106400030721     C                   ENDIF
106500030721     C*** NRS
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
106700030721     C                   EVAL      PiStr=SkSplitCSV(i)
106800030721     C                   EXSR      CHKNUM
106900030721     C                   IF        PiInt=*on AND
107000030721     C                             PiVal<=99
107100030721     C                   Z-ADD     PiVal         VABNRS
107200030721     C                   ELSE
107300030721     C                   SETON                                        32
107400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107500030721     C                             + ' ' + 'VABNRS'
107600030721     C                   ENDIF
107700030721     C                   ENDIF
107800030721     C*** LNA
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
108000030721     C                   EVAL      PiStr=SkSplitCSV(i)
108100030721     C                   EXSR      CHKNUM
108200030721     C                   IF        PiInt=*on    AND
108300030721     C                             PiVal<=999   AND
108400030721     C                             PiVal>*zeros
108500030721     C                   Z-ADD     PiVal         VABLNA
108600030721     C                   ELSE
108700030721     C                   SETON                                        32
108800030721     C                   EVAL      VABLNA = *zeros
108900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109000030721     C                             + ' ' + 'VABLNA'
109100030721     C                   ENDIF
109200030721     C                   ENDIF
109300030721     C*** CTR
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
109500030721     C                   EVAL      PiStr=SkSplitCSV(i)
109600030721     C                   EXSR      CHKNUM
109700030721     C                   IF        PiInt=*on  AND
109800030721     C                             PiVal<=999
109900030721     C                   Z-ADD     PiVal         VABCTR
110000030721     C                   ELSE
110100030721     C                   SETON                                        32
110200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110300030721     C                             + ' ' + 'VABCTR'
110400030721     C                   ENDIF
110500030721     C                   ENDIF
110600030721     C*** IAS
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
110800030721     C                   EVAL      PiStr=SkSplitCSV(i)
110900030721     C                   EXSR      CHKNUM
111000030721     C                   IF        PiNum=*on
111100030721     C                   Z-ADD     PiVal         VABIAS
111200030721     C                   ELSE
111300030721     C                   SETON                                        32
111400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111500030721     C                             + ' ' + 'VABIAS'
111600030721     C                   ENDIF
111700030721     C                   ENDIF
111800030721     C*** VLB
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiNum=*on
112300030721     C                   Z-ADD     PiVal         VABVLB
112400030721     C                   ELSE
112500030721     C                   SETON                                        32
112600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112700030721     C                             + ' ' + 'VABVLB'
112800030721     C                   ENDIF
112900030721     C                   ENDIF
113000030721     C*** QFT
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiNum=*on
113500030721     C                   Z-ADD     PiVal         VABQFT
113600030721     C                   ELSE
113700030721     C                   SETON                                        32
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABQFT'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** NCD
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiInt=*on      AND
114700030721     C                             PiVal<=9999999
114800030721     C                   Z-ADD     PiVal         VABNCD
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200030721     C                             + ' ' + 'VABNCD'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** NCA
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on      AND
116000030721     C                             PiVal<=9999999
116100030721     C                   Z-ADD     PiVal         VABNCA
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABNCA'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** ZNC
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiInt=*on AND
117300030721     C                             PiVal<=99
117400030721     C                   Z-ADD     PiVal         VABZNC
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABZNC'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** DCR
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on       AND
118600030721     C                             PiVal<=99999999
118700030721     C                   Z-ADD     PiVal         VABDCR
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABDCR'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** HCR
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiInt=*on   AND
119900030721     C                             PiVal<=9999
120000030721     C                   Z-ADD     PiVal         VABHCR
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400030721     C                             + ' ' + 'VABHCR'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** VMD
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiNum=*on
121200030721     C                   Z-ADD     PiVal         VABVMD
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600030721     C                             + ' ' + 'VABVMD'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** ANT
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on        AND
122400030721     C                             PiVal<=999999999
122500030721     C                   Z-ADD     PiVal         VABANT
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABANT'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** CMO
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiInt=*on
123700030721     C                   Z-ADD     PiVal         Num5_0
123800030721     C                   MOVEL(p)  Num5_0        VABCMO
123900030721     C                   ELSE
124000030721     C                   SETON                                        32
124100030721     C                   EVAL      VABCMO = *zeros
124200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124300030721     C                             + ' ' + 'VABCMO'
124400030721     C                   ENDIF
124500030721     C                   ENDIF
124600040510     C*
124700040510     C* Reperisco i campi del FIVAT
124800040510     C*** VATNOT
124900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
125000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
125100040510     C                   ENDIF
125200040510     C*** VATTRC
125300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
125400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
125500040510     C                   ENDIF
125600060331     C*** VATNOT_A
125700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
125800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
125900060331     C                   ENDIF
126000060331     C*** VATNOT_B
126100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
126200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
126300060331     C                   ENDIF
126400060331     C*** VATNOT_E
126500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
126600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
126700060331     C                   ENDIF
126800070823     C*
126900070823     C* Gestione campi "particolari"
127000070823     C*** RSD_W
127100070823     C                   IF        %trim(SkSplitFLD(i)) = 'RSD_W'
127200070823     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
127300070823     C                   EVAL      VABRSD = %trim(SkSplitCSV(i))
127400070823     C                   ENDIF
127500070823     C                   ENDIF
127600070823     C*** IND_W
127700070823     C                   IF        %trim(SkSplitFLD(i)) = 'IND_W'
127800070823     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
127900070823     C                   EVAL      VABIND = %trim(SkSplitCSV(i))
128000070823     C                   ENDIF
128100070823     C                   ENDIF
128200070823     C*** LOD_W
128300070823     C                   IF        %trim(SkSplitFLD(i)) = 'LOD_W'
128400070823     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
128500070823     C                   EVAL      VABLOD = %trim(SkSplitCSV(i))
128600070823     C                   ENDIF
128700070823     C                   ENDIF
128800070823     C*** CAD_W
128900070823     C                   IF        %trim(SkSplitFLD(i)) = 'CAD_W'
129000070823     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
129100070823     C                   EVAL      VABCAD = %trim(SkSplitCSV(i))
129200070823     C                   ENDIF
129300070823     C                   ENDIF
129400070823     C*** PRD_W
129500070823     C                   IF        %trim(SkSplitFLD(i)) = 'PRD_W'
129600070823     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
129700070823     C                   EVAL      VABPRD = %trim(SkSplitCSV(i))
129800070823     C                   ENDIF
129900070823     C                   ENDIF
130000070823     C*** TIC_W
130100070823     C                   IF        %trim(SkSplitFLD(i)) = 'TIC_W'
130200070823     C                   IF        %trim(SkSplitCSV(i)) = '1'
130300070823     C                   EVAL      VABTIC = *blanks
130400070823     C                   ENDIF
130500070823     C                   IF        %trim(SkSplitCSV(i)) = '2'
130600070823     C                   EVAL      VABTIC = 'CM'
130700070823     C                   ENDIF
130800070823     C                   IF        %trim(SkSplitCSV(i)) = '3'
130900070823     C                   EVAL      VABTIC = 'BM'
131000070823     C                   ENDIF
131100070823     C                   ENDIF
131200070823     C*** TC1_W
131300070823     C                   IF        %trim(SkSplitFLD(i)) = 'TC1_W'
131400070823     C                   IF        %trim(SkSplitCSV(i)) = 'SI'
131500070823     C                   EVAL      VABTC1 = 'P'
131600070823     C                   ENDIF
131700070823     C                   ENDIF
131800070823     C*** TC2_W
131900070823     C                   IF        %trim(SkSplitFLD(i)) = 'TC2_W'
132000070823     C                   IF        %trim(SkSplitCSV(i)) = 'SI'
132100070823     C                   EVAL      VABTC2 = 'A'
132200070823     C                   ENDIF
132300070823     C                   ENDIF
132400070823     C*** VABNSP
132500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
132600070823     C                   EVAL      PiStr=SkSplitCSV(i)
132700070823     C                   EXSR      CHKNUM
132800070823     C                   IF        PiInt=*on
132900070823     C                   Z-ADD     PiVal         VABNSP
133000070823     C                   ELSE
133100070823     C                   SETON                                        31
133200070823     C                   EVAL      VABNSP = *zeros
133300070823     C                   EVAL      vinmsg = %trimr(vinmsg)
133400070823     C                             + ' ' + 'VABNSP'
133500070823     C                   ENDIF
133600070823     C                   ENDIF
133700070823     C*** VABDCR
133800070823     C                   IF        %trim(SkSplitFLD(i)) = 'DCR_W'
133900070823     C                   EVAL      PiStr=SkSplitCSV(i)
134000070823     C                   EVAL      PiStr=%subst(PiStr:7:4)+%subst(PiStr:4:2)+
134100070823     C                                   %subst(PiStr:1:2)
134200070823     C                   EXSR      CHKNUM
134300070823     C                   IF        PiInt=*on       AND
134400070823     C                             PiVal<=99999999
134500070823     C                   Z-ADD     PiVal         VABDCR
134600070823     C                   ELSE
134700070823     C                   SETON                                        32
134800070823     C                   EVAL      vinmsg = %trimr(vinmsg)
134900070823     C                             + ' ' + 'VABDCR'
135000070823     C                   ENDIF
135100070823     C                   ENDIF
135200040510     C*
135300030715     C                   ENDIF
135400030715     C                   EVAL      i = i + 1
135500030715     C                   ENDDO
135600040510     C*
135700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
135800040510     C                   IF        VABNSP = *zeros
135900040510     C                   SETON                                        31
136000040510     C                   ENDIF
136100040715     C*
136200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
136300040715     C                   IF        VABLNP = *zeros
136400040715     C                   SETON                                        31
136500040715     C                   ENDIF
136600040510     C*
136700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
136800040510     C                   IF        VABRMN = *zeros
136900040510     C                   Z-ADD     VABNSP        VABRMN
137000040510     C                   ENDIF
137100040510     C*
137200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
137300040510     C                   IF        VABRMA = *blanks
137400040714     C                   MOVEL     VABRMN        VABRMA
137500040510     C                   ENDIF
137600040128     C*
137700040510     C* Se provincia nn valorizzata la reperisco
137800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
137900040510     C                   IF        VABPRD = *blanks
138000040128     C                   CLEAR                   TISI95DS
138100040128     C                   EVAL      I95TCN = '3'
138200040128     C                   Z-ADD     datcor        I95DAT
138300040128     C                   EVAL      I95NAR = VABNZD
138400040128     C                   EVAL      I95CAP = VABCAD
138500040128     C                   EVAL      I95LOC = VABLOD
138600040128     C                   CALL      'TISI95R'
138700040128     C                   PARM                    TISI95DS
138800040128     C                   EVAL      VABPRD = O95PRV
138900040510     C                   ENDIF
139000040113     C*
139100040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
139200040113     C                   IF        VABCAS > *zeros
139300040119     C                   EVAL      VABVCA = 'EUR'
139400040119     C                   IF        VABCBO = '1'
139500040113     C                   EVAL      VABCBO = '4'
139600040119     C                   ENDIF
139700040119     C                   IF        VABCBO = '2'
139800040119     C                   EVAL      VABCBO = '6'
139900040119     C                   ENDIF
140000040113     C                   ENDIF
140100020204     C*
140200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
140300020204     C                   EXSR      CHKIMPDIV
140400030715     C                   ENDIF
140500020204     C*
140600010607     C                   ENDSR
140700010607     C*----------------------------------------------------*
140800010601
140900020204
141000020204     C*----------------------------------------------------*
141100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
141200020204     C*----------------------------------------------------*
141300020204     C     CHKIMPDIV     BEGSR
141400020204     C*
141500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
141600020204     C                   Z-ADD     *zeros        wrkDec            9 9
141700020204     C*
141800020204     C* Come prima cosa effettuo considerazioni sulla divisa
141900020204     C                   IF        vabIAS > *zeros
142000020204     C                   IF        vabVAS <> 'EUR'
142100020204     C                   EVAL      vabVAS =  'ITL'
142200020204     C                   ENDIF
142300020204     C                   ENDIF
142400020204     C*
142500020204     C                   IF        vabCAS > *zeros
142600020204     C                   IF        vabVCA <> 'EUR'
142700020204     C                   EVAL      vabVCA =  'ITL'
142800020204     C                   ENDIF
142900020204     C                   ENDIF
143000020204     C*
143100020204     C                   IF        vabVMD > *zeros
143200020204     C                   IF        vabVAD <> 'EUR'
143300020204     C                   EVAL      vabVAD =  'ITL'
143400020204     C                   ENDIF
143500020204     C                   ENDIF
143600020204     C*
143700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
143800020204     C                   Z-ADD     vabIAS        wrkDec
143900020204     C                   IF        wrkDec > *zeros
144000020204     C                   IF        vabVAS = 'ITL'
144100020204     C                   EVAL      vabIAS = *zeros
144200020204     C                   ENDIF
144300020204     C                   ENDIF
144400020204     C*
144500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
144600020204     C                   Z-ADD     vabCAS        wrkDec
144700020204     C                   IF        wrkDec > *zeros
144800020204     C                   IF        vabVCA = 'ITL'
144900020204     C                   EVAL      vabCAS = *zeros
145000020204     C                   ENDIF
145100020204     C                   ENDIF
145200020204     C*
145300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
145400020204     C                   Z-ADD     vabVMD        wrkDec
145500020204     C                   IF        wrkDec > *zeros
145600020204     C                   IF        vabVAD = 'ITL'
145700020204     C                   EVAL      vabVMD = *zeros
145800020204     C                   ENDIF
145900020204     C                   ENDIF
146000020204     C*
146100020204     C                   ENDSR
146200020204     C***
146300020204
146400010330
146500010330
146600010330     C*----------------------------------------------------*
146700010330     C*  CONTROLLO NUMERICITA' CAMPI
146800010330     C*----------------------------------------------------*
146900010330     C     CHKNUM        BEGSR
147000010330     C*
147100010606     C                   IF        PiDecChr = *blanks
147200030715     C                   EVAL      PiDecChr = CharNUM
147300010606     C                   ENDIF
147400010606     C*
147500010606     C                   CALL(e)   'ISNUMERIC'
147600010330     C                   PARM                    PiStr            30
147700010606     C                   PARM                    PiDecChr          1
147800010330     C                   PARM      *ZEROS        PiVal            30 9
147900010330     C                   PARM      '0'           PiInt             1
148000010330     C                   PARM      '0'           PiNum             1
148100010330     C                   IF        %error
148200010606     C                   EVAL      PiNum=*off
148300010330     C                   ENDIF
148400010330     C*
148500010330     C                   ENDSR
148600010330     C***
148700010330
148800010601
148900010601
149000010601
149100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149200010601     C     repfil        BEGSR
149300010601     C*
149400010601     C                   if        invfil = *zeros and
149500010601     C                             depfil > *zeros and
149600010629     C                             (vinflg = *blanks or
149700010629     C                              vinflg = *zeros)
149800010601     C
149900010601     C                   eval      invfil = depfil
150000010601     C                   endif
150100010601     C*
150200010601     C                   if        depfil <> invfil and
150300010601     C                             invfil > *zeros
150400010601     C                   eval      flgMulti = '1'
150500010601     C                   if        vinflg = *blanks
150600010601     C                   add       1             cntNonEl
150700010601     C                   endif
150800010601     C                   endif
150900010601     C*
151000010601     C                   if        vinflg = '2'
151100010601     C                   eval      flgStato = '2'
151200010601     C                   endif
151300010601     C*
151400010601     C                   ENDSR
151500010601     C***
151600010601
151700010601
151800010601
151900010330
152000040119      /TITLE Invio dei dati al punto operativo.
152100040119     C     invio         BEGSR
152200040119     C*
152300040119     C* 1° invio FIVAT
152400040119     C                   reset                   dscmz
152500040119     C                   move      vlrpoi        cmzdst
152600040119     C                   eval      cmzfld = 'FIVATWWR'
152700040119     C                   eval      cmzmbd = vlrhdl
152800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
152900040119     C***                if        prmfir = *blanks
153000040119     C                   eval      cmzfla = 'FIVAT00F'
153100040119     C                   eval      cmzmba = 'FIVAT00F'
153200040119     C***                else
153300040119     C***                eval      cmzfla = prmfir
153400040119     C***                eval      cmzmba = prmfir
153500040119     C***                endif
153600040119     C                   eval      cmznrr = *zeros
153700040119     C                   move      §ctrokvt      cmznrr
153800040119     C                   eval      cmzlba = vlrfl1
153900040119     C                   call(e)   'TIS711C'
154000040119     C                   parm                    dscmz
154100040119     C                   parm      *blanks       esito
154200040119     C                   if        %error
154300040119     C                             or cmzerr = '1'
154400040119     C                             or esito  = '1'
154500040119     C                   eval      wrkesito = '3'
154600040119     C                   else
154700040119     C*
154800040119     C* 2° invio FIVAB
154900040119     C                   reset                   dscmz
155000040119     C                   move      vlrpoi        cmzdst
155100040119     C                   eval      cmzfld = vlrfou
155200040119     C                   eval      cmzmbd = vlrhdl
155300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
155400040119     C***                if        prmfir = *blanks
155500040119     C                   eval      cmzfla = 'FIVAB00F'
155600040119     C                   eval      cmzmba = 'FIVAB00F'
155700040119     C***                else
155800040119     C***                eval      cmzfla = prmfir
155900040119     C***                eval      cmzmba = prmfir
156000040119     C***                endif
156100040119     C                   eval      cmznrr = *zeros
156200040119     C                   move      §ctrokvb      cmznrr
156300040119     C                   eval      cmzlba = vlrfl1
156400040119     C                   call(e)   'TIS711C'
156500040119     C                   parm                    dscmz
156600040119     C                   parm      *blanks       esito
156700040119     C                   if        %error
156800040119     C                             or cmzerr = '1'
156900040119     C                             or esito  = '1'
157000040119     C                   eval      wrkesito = '3'
157100040119     C                   endif
157200040119     C                   endif
157300040119     C*
157400040119     C                   ENDSR
157500040119     C***
157600010601
157700010601
157800010601
157900010601
158000010601      /TITLE Invio dei dati al punto operativo.
158100010601     C     opeini        BEGSR
158200010601     C*
158300010601     C* Inizializzo flag e contatori operativi
158400010601     C                   movel     '0'           flgGiro           1
158500010601     C                   movel     '0'           flgMulti          1
158600010601     C                   movel     '1'           flgStato          1
158700010615     C                   movel     '0'           flgOk             1
158800010601     C                   z-add     *zeros        cntNonEl         10 0
158900010601     C                   z-add     *zeros        depfil            3 0
159000010601     C                   z-add     *zeros        invfil            3 0
159100010601     C*
159200010601     C                   ENDSR
159300010601     C***
159400070326
159500070326
159600070326
159700070326
159800070326     C     *pssr         BEGSR
159900070329     C*
160000070329     C                   if        %open(tivin00r)
160100070329     C                   close     tivin00r
160200070329     C                   endif
160300070329     C                   if        %open(fivabwwr)
160400070329     C                   close     fivabwwr
160500070329     C                   endif
160600070329     C                   if        %open(fivatwwr)
160700070329     C                   close     fivatwwr
160800070329     C                   endif
160900070326     C*
161000070326     C* Effettuo la chiamata al CLLE preposto
161100070326     C                   call(e)   'TITVVTC'
161200070326     C                   parm                    parccm
161300070326     C                   parm                    parmbr
161400070326     C                   parm      '2'           paropz
161500070326     C*
161600070326     C                   eval      wrkesito = '2'
161700070404     C*
161800070404     C                   seton                                        LR
161900070326     C*
162000070326     C                   ENDSR     '*CANCL'
162100070326     C***
162200070326
162300070326
162400010330
162500010330
162600000613     C     *inzsr        BEGSR
162700990910     C*
162800990910     C     *entry        plist
162900990920     C                   parm                    tivlrds
163000990921     C                   parm      wrkesito      esito
163100000724     C                   parm                    prmlit
163200000710     C                   parm                    prmfir
163300010330     C*
163400010330     C* CALCOLA LA DATA CORRENTE
163500010330     C                   time                    wn14             14 0
163600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
163700010330     C                   z-add     wn8           g08dat
163800010330     C                   z-add     *zeros        g08inv
163900010330     C                   movel     '0'           g08err
164000010330     C                   call      'XSRDA8'
164100010330     C                   parm                    wlbda8
164200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
164300000613     C*
164400000613     C                   ENDSR
164500000613     C***
164600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
164700070823VABRSD;VABRD2;VABIND;VABLOD;VABCAD;VABPRD;RSD_W;IND_W;LOD_W;CAD_W;VABNOT;
164800070823VABNT2;VABCAS;TIC_W;VABIAS;VABCBO;TC1_W;TC2_W;DCR_W;FILL1;FILL2;VABRMN;
164900070823FILL3;VABNCL;VABPKB;FILL4;FILL5;FILL6;FILL7;FILL8;FILL9;VATNOT_E
