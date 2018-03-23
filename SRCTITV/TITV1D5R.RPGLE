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
003000080125     D wFlgCAS         s              1    INZ(*blanks)
003100030715     D*------------------
003200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003300030715     D*------------------
003400030715     D SkSplitFLD      S             10    DIM(100)
003500030715     D SkSplitCSV      S            256    DIM(100)
003600030715     D CharCSV         S              1
003700030715     D CharTXT         S              1
003800030715     D CharNUM         S              1
003900030715     D posDa           S              3  0 INZ(*zeros)
004000030715     D posA            S              3  0 INZ(*zeros)
004100030715     D i               s              3  0 INZ(1)
004200030715     D wGiro           s              1  0 INZ(*zeros)
004300080125     D wVATNOT_A       s                   LIKE(VATNOT)
004400080125     D wVATNOT_B       s                   LIKE(VATNOT)
004500080125     D wVATNOT_E       s                   LIKE(VATNOT)
004600030822     D*------------------
004700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004800030822     D*------------------
004900030822     D posDaDft        S              3  0 INZ(*zeros)
005000030822     D posADft         S              3  0 INZ(*zeros)
005100030822     D j               s              3  0 INZ(1)
005200030822     D wGiroDft        s              1  0 INZ(*zeros)
005300010330     D*------------------
005400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500010330     D*------------------
005600010330     D WLBDA8          DS                  INZ
005700010330     D  G08DAT                 1      8  0
005800010330     D  G08INV                 9     16  0
005900010330     D  G08ERR                17     17
006000010330     D  G08TGI                18     22  0
006100031201     D*------------------
006200031201     D* Costanti
006300031201     D*------------------
006400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006600010330
006700010330
006800010330
006900000913     C                   reset                   rrnum
007000990921     C                   reset                   esito
007100990921     C                   reset                   wrkesito
007200010601     C*
007300010601     C                   exsr      opeini
007400010605     C                   exsr      rwvab
007500010601     C*
007600010601     C                   seton                                        lr
007700010601
007800010601
007900010601
008000010601     C*--------------------------------------------------------
008100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008200010601     C*--------------------------------------------------------
008300010601     C     PREELA        BEGSR
008400010601     C*
008500010601     C* SE OCCORRE SPEDIRE IN FILIALE
008600010601     C                   if        invfil <> *zeros and
008700010601     C                             flgGiro = '0'
008800010601     C*
008900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009000010601     C                   eval      flgGiro = '1'
009100010601     C*
009200010601     C                   endif
009300010601     C*
009400010601     C                   ENDSR
009500010601     C***
009600010601
009700010601
009800010601
009900010601
010000010601     C*--------------------------------------------------------
010100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010200010601     C*--------------------------------------------------------
010300010601     C     ENDELA        BEGSR
010400040119     C*
010500040119     C* Effettuo la chiamata al CLLE preposto
010600040119     C                   call(e)   'TITVVTC'
010700040510     C                   parm                    parccm
010800040119     C                   parm                    parmbr
010900040119     C                   parm      '2'           paropz
011000050201     C*
011100050201     C* Effettuo lancio TISI95 solo x chiusura
011200050201     C                   CLEAR                   TISI95DS
011300050201     C                   EVAL      I95TLA = 'C'
011400050201     C                   CALL      'TISI95R'
011500050201     C                   PARM                    TISI95DS
011600000616     C*
011700010601     C                   ENDSR
011800010601     C***
011900000613
012000010601
012100010601
012200010330     C*--------------------------------------------------------
012300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
012400010330     C*--------------------------------------------------------
012500010605     C     RWVAB         BEGSR
012600010330     C*
012700010330     C                   if        not %open(tivin00r)
012800010330     C                   open      tivin00r
012900010330     C                   endif
013000021025     C                   if        not %open(fivabwwr)
013100021025     C                   open      fivabwwr
013200010330     C                   endif
013300040119     C*
013400040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013500040119     C                   exsr      prevat
013600040119     C*
013700040119     C                   if        chkcall = '0'
013800040119     C*
013900040119     C                   if        not %open(fivatwwr)
014000040119     C                   open      fivatwwr
014100040119     C                   endif
014200010330     C*
014300040119     C                   clear                   §CTROKVB          7 0
014400040119     C                   clear                   §CTROKVT          7 0
014500040119     C                   clear                   §CTRMO            7 0
014600040119     C                   clear                   §CTRNO            7 0
014700070823     C*
014800070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
014900070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
015000080108     C***                exsr      inzvar
015100080108     C***                exsr      impvab
015200010330     C*
015300030822     C                   do        *HIVAL
015400010330     C*
015500030822     C                   read      tivin00r                               70
015600010618     C*
015700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015800010618     C                   if        vinflg = '1'
015900010618     C                   eval      flgOk = '1'
016000010618     C                   endif
016100040510     C*
016200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016300040510     C                   z-add     *zeros        wLenVINDTA        4 0
016400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
016600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016800040510     C                   z-add     1             wFlgVINDTA
016900040510     C                   leave
017000040510     C                   endif
017100040510     C                   enddo
017200010618     C*
017300040510     C                   if        vindta > *blanks AND
017400040510     C                             wFlgVINDTA = 1
017500010330     C                   add       1             rrnum
017600010330     C*
017700010601     C                   if        *in70 = *off and
017800010330     C                             (vinflg = *blanks
017900010330     C                              or vinflg = '0'
018000010330     C                              or vinflg = '2')
018100010330     C*
018200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018300010711     C                   if        vinflg = *blanks or vinflg = '0'
018400010711     C                   clear                   vinmsg
018500010711     C                   endif
018600010601     C*
018700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
018800070925     C                   eval      vindta  = ' ' + %trim(vindta)
018900070925     C*
019000010330     C                   exsr      inzvar
019100010605     C                   exsr      impvab
019200010601     C*
019300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019400010605     C                   eval      depfil = VABLNP
019500010601     C                   exsr      repfil
019600010601     C                   if        depfil = invfil
019700021025     C                   if        vlrpoi = 999
019800030822     C                   move(P)   invfil        VABFGS
019900021025     C                   else
020000030822     C                   move(P)   vlrpoi        VABFGS
020100021025     C                   endif
020200010601     C*
020300010601     C                   exsr      PREELA
020400010601     C*
020500010604     C* Ebbene...
020600010604     C*
020700040119     C  N31              add       1             §CTROKVB
020800040119     C   32              add       1             §CTRMO
020900040119     C   31              add       1             §CTRNO
021000030822     C                   if        wGiro = 2
021100040510     C* Scrivo sempre il VAT
021200040510     C  N31              exsr      wrivat
021300040510     C* Scrivo il VAB solo a rottura d spedizione
021400040119     C                   if        depspe =  *zeros OR
021500040119     C                             depspe <> vabNSP
021600040119     C  N31              exsr      wrivab
021700040119     C  N31              eval      depspe = vabNSP
021800040119     C                   endif
021900040119     C                   endif
022000010604     C*
022100010604     C                   if        *in31 = *off and
022200010604     C                             *in32 = *off
022300010604     C                   eval      vinflg = '1'
022400010604     C                   else
022500010604     C                   eval      vinflg = '2'
022600010604     C                   endif
022700010604     C                   endif
022800010601     C*
022900010604     C                   endif
023000010604     C*
023100010330     C                   else
023200010330     C                   eval      vinflg = '1'
023300010330     C                   endif
023400010601     C*
023500010601     C  N70              update    tivin000
023600010330     C*
023700030822     C  N70              enddo
023800040119     C*
023900040119     C                   endif                                                  (endif - chkcall)
024000010601     C*
024100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024200010601     C                   if        cntNonEl = *zeros or
024300010601     C                             flgMulti = '0'
024400010330     C* Se non ci sono record con errori ...
024500010601     C                   if        §ctrno = 0 and
024600010604     C                             §ctrmo = 0 and
024700010601     C                             flgStato <> '2'
024800010330     C* ... restituisco esito OK.
024900010330     C                   eval      wrkesito = '0'
025000010330     C                   else
025100040119     C                   if        §ctrokvb > 0
025200010330     C                   eval      wrkesito = '1'
025300010330     C                   else
025400010615     C                   if        flgOk = '0'
025500010615     C                   eval      wrkesito = '2'
025600010615     C                   else
025700010615     C                   eval      wrkesito = '6'
025800010615     C                   endif
025900010330     C                   endif
026000010330     C                   endif
026100010601     C                   else
026200010601     C                   eval      wrkesito = '9'
026300010601     C                   endif
026400010330     C*
026500010330     C                   if        %open(tivin00r)
026600010330     C                   close     tivin00r
026700010330     C                   endif
026800021025     C                   if        %open(fivabwwr)
026900021025     C                   close     fivabwwr
027000010330     C                   endif
027100040512     C                   if        %open(fivatwwr)
027200040512     C                   close     fivatwwr
027300040512     C                   endif
027400010601     C*
027500010601     C                   if        vlrpoi <> 999
027600010601     C                   eval      invfil = vlrpoi
027700010601     C                   endif
027800010330     C*
027900040119     C                   if        §ctrokvb > 0
028000010601     C                             and invfil > *zeros
028100010330     C                   exsr      invio
028200010330     C                   endif
028300010601     C*
028400010618     C                   if        flgGiro = '1'
028500010601     C                   exsr      endela
028600010618     C                   endif
028700010330     C*
028800010330     C                   ENDSR
028900010330     C***
029000040119
029100040119
029200040119
029300040119     C*----------------------------------------------------*
029400040119     C*  SCARICAMENTO BUFFER RECORDS VAB
029500040119     C*----------------------------------------------------*
029600040510     C     WRIVAB        BEGSR
029700040119     C*
029800060331     C* Gestisco l'eventuale rottura x numero spedizione
029900070823     C                   if        VABNSP <> depspe
030000070301     C                   if        wVATNOT_E <> *blanks
030100080125     C                   if        VABCTM = *blanks
030200080125     C                   eval      VABCTM = '7Q'
030300080125     C                   endif
030400070301     C                   endif
030500040119     C                   write     fivab000                                     => scarico il VAB
030600060331     C                   endif
030700040119     C*
030800040119     C                   ENDSR
030900040119
031000040119
031100040119
031200040119     C*----------------------------------------------------*
031300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
031400040119     C*----------------------------------------------------*
031500040510     C     WRIVAT        BEGSR
031600060512     C*
031700060512     C* Inizializzo i campi chiave
031800060512     C                   eval      VATFGS = VABFGS
031900060512     C                   eval      VATAAS = VABAAS
032000060512     C                   eval      VATCCM = VABCCM
032100070823     C                   eval      VATNRS = VABNRS
032200060512     C                   eval      VATNSP = VABNSP
032300060512     C                   eval      VATLNP = VABLNP
032400040714     C*
032500040714     C* Solo se VATNOT valorizzato scarico il buffer
032600040714     C                   if        VATNOT <> *blanks
032700040119     C*
032800040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
032900040510     C                   if        VATTRC = *blanks
033000040510     C                   eval      VATTRC = 'E'
033100040510     C                   endif
033200040510     C                   write     FIVAT000
033300040119     C                   add       1             §CTROKVT
033400040714     C*
033500040714     C                   endif
033600060331     C*
033700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
033800060331     C                   if        wVATNOT_A <> *blanks
033900060331     C                   eval      VATNOT = wVATNOT_A
034000060331     C                   eval      VATTRC = 'A'
034100060331     C                   write     FIVAT000
034200060331     C                   add       1             §CTROKVT
034300060331     C                   endif
034400060331     C*
034500060331     C                   if        wVATNOT_B <> *blanks
034600060331     C                   eval      VATNOT = wVATNOT_B
034700060331     C                   eval      VATTRC = 'B'
034800060331     C                   write     FIVAT000
034900060331     C                   add       1             §CTROKVT
035000060331     C                   endif
035100060331     C*
035200060331     C                   if        wVATNOT_E <> *blanks
035300060331     C                   eval      VATNOT = wVATNOT_E
035400060331     C                   eval      VATTRC = 'E'
035500060331     C                   write     FIVAT000
035600060331     C                   add       1             §CTROKVT
035700060331     C                   endif
035800040119     C*
035900040119     C                   ENDSR
036000040119
036100040119
036200040119
036300040119     C*----------------------------------------------------*
036400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
036500040119     C*----------------------------------------------------*
036600040119     C     PREVAT        BEGSR
036700040119     C*
036800040119     C* Compongo il nome del membro da dare al FIVATWWR
036900040119     C                   eval      parmbr = vlrhdl
037000040119     C                   movel     'M'           parmbr
037100040510     C                   eval      parccm = vlrksc
037200040119     C                   eval      paropz = '1'
037300040119     C* Effettuo la chiamata al CLLE preposto
037400040119     C                   call(e)   'TITVVTC'
037500040119     C                   parm                    parccm
037600040119     C                   parm                    parmbr
037700040119     C                   parm                    paropz
037800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
037900040119     C                   if        %error
038000040119     C                   movel     '1'           chkcall
038100040119     C                   else
038200040119     C                   movel     '0'           chkcall
038300040119     C                   endif
038400040119     C*
038500040119     C                   ENDSR
038600010601
038700010601
038800010601
038900010330     C*----------------------------------------------------*
039000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
039100010330     C*----------------------------------------------------*
039200040119     C     INZVAR        BEGSR
039300010330     C*
039400030822     C* Inizializzo variabili di wrk
039500020204     C                   Z-ADD     *zeros        Num5_0            5 0
039600060331     C                   MOVEL     *blanks       wVATNOT_A
039700060331     C                   MOVEL     *blanks       wVATNOT_B
039800060331     C                   MOVEL     *blanks       wVATNOT_E
039900080925     C                   MOVEL     *blanks       wFlgCAS
040000030822     C*
040100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
040200030822     C                   CLEAR                   FIVAB000
040300040119     C                   CLEAR                   FIVAT000
040400030822     C                   CLEAR                   SkSplitCSV
040500030822     C                   EVAL      i = 1
040600030822     C                   EVAL      posDa = *zeros
040700030822     C                   EVAL      posA  = *zeros
040800030822     C*
040900030822     C* Reimposto i valori di default
041000030822     C                   EXSR      DEFCAM
041100030822     C*
041200010330     C                   ENDSR
041300010330     C*----------------------------------------------------*
041400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
041500010330     C*----------------------------------------------------*
041600010330     C     DEFCAM        BEGSR
041700030822     C*
041800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
041900030715     C* e delimitatore testo.
042000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
042100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
042200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
042300030822     C*
042400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
042500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
042600030715     C                   SETON                                        50
042700030715     C                   ELSE
042800030715     C                   SETOFF                                       50
042900030715     C                   ENDIF
043000030822     C*
043100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
043200030822     C                   EVAL      posDaDft = 1
043300030822     C                   EVAL      posADft  = 0
043400030822     C                   EVAL      wGiroDft = 0
043500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
043600030822     C                             posDaDft > 0
043700030822     C*
043800030822     C* Gestisco il 1° giro
043900030822     C                   IF        wGiroDft = 0
044000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
044100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
044200030822     C* Incremento il contatore dei "giri"
044300030822     C                   EVAL      wGiroDft = 1
044400030822     C                   ELSE
044500030822     C                   EVAL      posDaDft = posADft
044600030822     C                   ENDIF
044700030822     C* Eseguo lo scan x trovare la fine del campo corrente
044800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
044900030822     C*
045000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
045100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
045200030822     C                   IF        posDaDft > 0 AND
045300030822     C                             posADft  > 0
045400030822     C* NCL
045500030822     C                   IF        %subst(
045600030822     C                             %subst(vlrppt:posDaDft+1:
045700030822     C                             posADft-posDaDft-1):1:3)
045800030822     C                             = 'NCL'
045900030822     C                   EVAL      PiStr=%trim(%subst(
046000030822     C                             %subst(vlrppt:posDaDft+1:
046100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046200030822     C                   EXSR      CHKNUM
046300030822     C                   IF        PiInt=*on
046400030822     C                   Z-ADD     PiVal         VABNCL
046500030822     C                   ENDIF
046600030822     C                   ENDIF
046700030822     C* CCM
046800030822     C                   IF        %subst(
046900030822     C                             %subst(vlrppt:posDaDft+1:
047000030822     C                             posADft-posDaDft-1):1:3)
047100030822     C                             = 'CCM'
047200030822     C                   EVAL      PiStr=%trim(%subst(
047300030822     C                             %subst(vlrppt:posDaDft+1:
047400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047500030822     C                   EXSR      CHKNUM
047600030822     C                   IF        PiInt=*on
047700030822     C                   Z-ADD     PiVal         VABCCM
047800030822     C                   ENDIF
047900030822     C                   ENDIF
048000030822     C* LNP
048100030822     C                   IF        %subst(
048200030822     C                             %subst(vlrppt:posDaDft+1:
048300030822     C                             posADft-posDaDft-1):1:3)
048400030822     C                             = 'LNP'
048500030822     C                   EVAL      PiStr=%trim(%subst(
048600030822     C                             %subst(vlrppt:posDaDft+1:
048700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048800030822     C                   EXSR      CHKNUM
048900030822     C                   IF        PiInt=*on
049000030822     C                   Z-ADD     PiVal         VABLNP
049100030822     C                   ENDIF
049200030822     C                   ENDIF
049300030822     C* NRS
049400030822     C                   IF        %subst(
049500030822     C                             %subst(vlrppt:posDaDft+1:
049600030822     C                             posADft-posDaDft-1):1:3)
049700030822     C                             = 'NRS'
049800030822     C                   EVAL      PiStr=%trim(%subst(
049900030822     C                             %subst(vlrppt:posDaDft+1:
050000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050100030822     C                   EXSR      CHKNUM
050200030822     C                   IF        PiInt=*on
050300030822     C                   Z-ADD     PiVal         VABNRS
050400030822     C                   ENDIF
050500030822     C                   ENDIF
050600030822     C* CTR
050700030822     C                   IF        %subst(
050800030822     C                             %subst(vlrppt:posDaDft+1:
050900030822     C                             posADft-posDaDft-1):1:3)
051000030822     C                             = 'CTR'
051100030822     C                   EVAL      PiStr=%trim(%subst(
051200030822     C                             %subst(vlrppt:posDaDft+1:
051300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051400030822     C                   EXSR      CHKNUM
051500030822     C                   IF        PiInt=*on
051600030822     C                   Z-ADD     PiVal         VABCTR
051700030822     C                   ENDIF
051800030822     C                   ENDIF
051900030822     C* PKB
052000030822     C                   IF        %subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):1:3)
052300030822     C                             = 'PKB'
052400030822     C                   EVAL      PiStr=%trim(%subst(
052500030822     C                             %subst(vlrppt:posDaDft+1:
052600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052700030822     C                   EXSR      CHKNUM
052800030822     C                   IF        PiNum=*on
052900030822     C                   Z-ADD     PiVal         VABPKB
053000030822     C                   ENDIF
053100030822     C                   ENDIF
053200030822     C* VLB
053300030822     C                   IF        %subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):1:3)
053600030822     C                             = 'VLB'
053700030822     C                   EVAL      PiStr=%trim(%subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054000030822     C                   EXSR      CHKNUM
054100030822     C                   IF        PiNum=*on
054200030822     C                   Z-ADD     PiVal         VABVLB
054300030822     C                   ENDIF
054400030822     C                   ENDIF
054500030822     C* QFT
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'QFT'
055000030822     C                   EVAL      PiStr=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   EXSR      CHKNUM
055400030822     C                   IF        PiNum=*on
055500030822     C                   Z-ADD     PiVal         VABQFT
055600030822     C                   ENDIF
055700030822     C                   ENDIF
055800030822     C* CBO
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'CBO'
056300030822     C                   EVAL      VABCBO=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   ENDIF
056700030822     C* TSP
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'TSP'
057200030822     C                   EVAL      VABTSP=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   ENDIF
057600030822     C* VAS
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'VAS'
058100030822     C                   EVAL      VABVAS=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   ENDIF
058500030822     C* VCA
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'VCA'
059000030822     C                   EVAL      VABVCA=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   ENDIF
059400070925     C* TIC
059500070925     C                   IF        %subst(
059600070925     C                             %subst(vlrppt:posDaDft+1:
059700070925     C                             posADft-posDaDft-1):1:3)
059800070925     C                             = 'TIC'
059900070925     C                   EVAL      VABTIC=%trim(%subst(
060000070925     C                             %subst(vlrppt:posDaDft+1:
060100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200070925     C                   ENDIF
060300030822     C* GCA
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'GCA'
060800030822     C                   EVAL      VABGCA=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   ENDIF
061200030822     C* CTM
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'CTM'
061700030822     C                   EVAL      VABCTM=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   ENDIF
062100030822     C* FFD
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'FFD'
062600030822     C                   EVAL      VABFFD=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   ENDIF
063000030822     C* VAD
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'VAD'
063500030822     C                   EVAL      VABVAD=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   ENDIF
063900030822     C* GMA
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'GMA'
064400030822     C                   EVAL      VABGMA=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   ENDIF
064800030822     C* GGA
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'GGA'
065300030822     C                   EVAL      VABGGA=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* GVA
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'GVA'
066200030822     C                   EVAL      VABGVA=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* TC1
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'TC1'
067100030822     C                   EVAL      VABTC1=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* TC2
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'TC2'
068000030822     C                   EVAL      VABTC2=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400040714     C* VATTRC
068500040714     C                   IF        %subst(
068600040714     C                             %subst(vlrppt:posDaDft+1:
068700040714     C                             posADft-posDaDft-1):1:3)
068800040714     C                             = 'TRC'
068900040714     C                   EVAL      VATTRC=%trim(%subst(
069000040714     C                             %subst(vlrppt:posDaDft+1:
069100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200040714     C                   ENDIF
069300030822     C* ...
069400030822     C                   ENDIF
069500030822     C                   ENDDO
069600020204     C*
069700010330     C                   ENDSR
069800010607     C*----------------------------------------------------*
069900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
070000010607     C*----------------------------------------------------*
070100010607     C     IMPVAB        BEGSR
070200010607     C*
070300010607     C                   SETOFF                                       3132
070400030515     C*
070500010607     C                   MOVEL     datcor        VABAAS
070600010607     C                   MOVE      datcor        VABMGS
070700010607     C*
070800030715     C**********
070900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
071000030715     C**********
071100030715     C                   IF        wGiro = *zeros
071200040113     C*
071300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
071400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
071500040510     C                             %trim(intcol(3))<>*blanks
071600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
071700040113     C                                      %trim(intcol(3))
071800070823     C     ';':CharCSV   XLATE     vindta        vindta
071900040510     C                   ENDIF
072000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
072100031201     C     minu:maiu     XLATE     vindta        vindta
072200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
072300030715     C                   DOW       posDa <= %len(%trim(vindta))
072400030715     C*
072500030715     C* Gestisco il 1° campo
072600030715     C                   IF        i = 1
072700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
072800030715     C                   EVAL      posDa = 1
072900030715     C* Eseguo lo scan x trovare la fine del primo campo
073000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
073100030715     C* A questo punto "estraggo" il campo corrente
073200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073300030715     C                                                    (posA-posDa))
073400030715     C* X i campi successivi al 1°
073500030715     C                   ELSE
073600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
073700030715     C                   EVAL      posDa = posA + 1
073800030715     C* Eseguo lo scan x trovare la fine del campo corrente
073900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074000030715     C* Gestisco l'ultimo campo
074100030715     C                   IF        posA = *zeros
074200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
074300030715     C* A questo punto "estraggo" il campo corrente
074400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
074500030715     C                                                    (posA-posDa))
074600030715     C                   LEAVE
074700030715     C                   ELSE
074800030715     C* A questo punto "estraggo" il campo corrente
074900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075000030715     C                                                    (posA-posDa))
075100030715     C                   ENDIF
075200030715     C                   ENDIF
075300030715     C* Incremento il contatore d campo
075400030715     C                   EVAL      i = i +1
075500030715     C                   ENDDO
075600030715     C*
075700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
075800030715     C* delimitazione testo
075900030715     C                   EVAL      i = 1
076000030715     C                   DOW       i < %elem(SkSplitFLD)
076100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
076200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
076300030715     C                   EVAL      i = i + 1
076400030715     C                   ENDDO
076500030715     C                   EVAL      wGiro = 1
076600030715     C                   ELSE
076700030715     C                   EVAL      wGiro = 2
076800031204     C**********
076900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
077000031204     C* d separatore campo
077100031204     C**********
077200031204     C                   EVAL      vindta = %trim(vindta)
077300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
077400031204     C                   EVAL      vindta = %subst(vindta:2)
077500031204     C                   ENDDO
077600031204     C*
077700031204     C                   Z-ADD     *zeros        lunghInput        4 0
077800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
077900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
078000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
078100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
078200031204     C                   ENDDO
078300030715     C**********
078400030715     C* Effettuo lo split del campo dati d input
078500030715     C**********
078600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078700030715     C                   DOW       posDa <= %len(%trim(vindta))
078800030715     C*
078900030715     C* Gestisco il 1° campo
079000030715     C                   IF        i = 1
079100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079200030715     C                   EVAL      posDa = 1
079300030715     C* Eseguo lo scan x trovare la fine del primo campo
079400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079500030715     C* A questo punto "estraggo" il campo corrente
079600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079700030715     C                                                    (posA-posDa))
079800030715     C* X i campi successivi al 1°
079900030715     C                   ELSE
080000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080100030715     C                   EVAL      posDa = posA + 1
080200031204     C* Verifico che nn vi sia il campo nn valorizzato
080300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
080400031204     C* Se campo nn valorizzato skippo al prossimo
080500031204     C                   EVAL      SkSplitCSV(i) = *blanks
080600031204     C                   EVAL      posA  = posA + 1
080700031204     C                   ELSE
080800030715     C* Eseguo lo scan x trovare la fine del campo corrente
080900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081000030715     C* Gestisco l'ultimo campo
081100030715     C                   IF        posA = *zeros
081200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081300030715     C* A questo punto "estraggo" il campo corrente
081400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081500030715     C                                                    (posA-posDa))
081600030715     C                   LEAVE
081700030715     C                   ELSE
081800030715     C* A questo punto "estraggo" il campo corrente
081900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082000030715     C                                                    (posA-posDa))
082100030715     C                   ENDIF
082200031204     C                   ENDIF
082300030715     C                   ENDIF
082400030715     C* Incremento il contatore d campo
082500030715     C                   EVAL      i = i +1
082600030715     C                   ENDDO
082700030715     C*
082800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082900030715     C* delimitazione testo
083000030715     C                   EVAL      i = 1
083100030715     C                   DOW       i < %elem(SkSplitCSV)
083200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
083300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
083400030715     C                   EVAL      i = i + 1
083500030715     C                   ENDDO
083600030715     C*
083700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
083800030715     C                   EVAL      i = 1
083900030715     C                   DOW       i < %elem(SkSplitFLD)
084000030715     C*
084100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
084200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
084300030715     C                             SkSplitCSV(i) <> *blanks
084400030721     C*** ATB
084500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
084600030721     C                   EVAL      VABATB = SkSplitCSV(i)
084700030721     C                   ENDIF
084800030715     C*** RSD
084900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
085000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
085100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
085200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
085300030715     C* ==
085400030715     C                   ENDIF
085500030721     C*** RD2
085600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
085700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
085800030721     C                   ENDIF
085900030715     C*** IND
086000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
086100030715     C                   EVAL      VABIND = SkSplitCSV(i)
086200030715     C                   ENDIF
086300030715     C*** LOD
086400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
086500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
086600030715     C                   ENDIF
086700030715     C*** PRD
086800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
086900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
087000030715     C                   ENDIF
087100030721     C*** NZD
087200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
087300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
087400030721     C                   ENDIF
087500030715     C*** RMA
087600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
087700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
087800030715     C                   ENDIF
087900030715     C*** NOT
088000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
088100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
088200030715     C                   ENDIF
088300030715     C*** NT2
088400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
088500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
088600030715     C                   ENDIF
088700030715     C*** VCA
088800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
088900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
089000030715     C                   ENDIF
089100030715     C*** VAS
089200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
089300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
089400030715     C                   ENDIF
089500030715     C*** FFD
089600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
089700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
089800030715     C                   ENDIF
089900030721     C*** GC1
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
090100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** GC2
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
090500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** TSP
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
090900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
091000030721     C                   ENDIF
091100040510     C*** CBO
091200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
091300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
091400040510     C                   ENDIF
091500030721     C*** NAS
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
091700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030721     C*** TIC
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
092100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** GCA
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
092500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030721     C*** XCO
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
092900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** CTM
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
093300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** TCR
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
093700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*** CTS
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
094100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030721     C*** FTM
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
094500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*** VAD
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
094900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*** GMA
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
095300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030721     C*** GGA
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
095700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030721     C*** GVA
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
096100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** TC1
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
096500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*** TC2
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
096900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030721     C*** SCL
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
097300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
097400030721     C                   ENDIF
097500030721     C*** RMO
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
097700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030721     C*** NMO
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
098100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030721     C*
098400030721     C* Reperisco quindi i campi numerici...
098500040510     C*** NSP
098600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
098700040510     C                   EVAL      PiStr=SkSplitCSV(i)
098800040510     C                   EXSR      CHKNUM
098900040510     C                   IF        PiInt=*on
099000040510     C                   Z-ADD     PiVal         VABNSP
099100040510     C                   ELSE
099200040510     C                   SETON                                        31
099300040510     C                   EVAL      VABNSP = *zeros
099400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
099500040510     C                             + ' ' + 'VABNSP'
099600040510     C                   ENDIF
099700040510     C                   ENDIF
099800071106     C*** RMN
099900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
100000071106     C                   EVAL      PiStr=SkSplitCSV(i)
100100071106     C                   EXSR      CHKNUM
100200071106     C                   IF        PiInt=*on
100300071106     C                   Z-ADD     PiVal         VABRMN
100400071106     C                   ELSE
100500071106     C                   SETON                                        32
100600071106     C                   EVAL      VABRMN = *zeros
100700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
100800071106     C                             + ' ' + 'VABRMN'
100900071106     C                   ENDIF
101000071106     C                   ENDIF
101100030721     C*** CAD
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
101300030721     C                   EVAL      PiStr=SkSplitCSV(i)
101400030721     C                   EXSR      CHKNUM
101500030721     C                   IF        PiInt=*on
101600030721     C                   Z-ADD     PiVal         Num5_0
101700030721     C                   MOVEL(p)  Num5_0        VABCAD
101800030721     C                   ELSE
101900030721     C                   SETON                                        32
102000030721     C                   EVAL      VABCAD = *zeros
102100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102200030721     C                             + ' ' + 'VABCAD'
102300030721     C                   ENDIF
102400030721     C                   ENDIF
102500030721     C*** NCL
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
102700030721     C                   EVAL      PiStr=SkSplitCSV(i)
102800030721     C                   EXSR      CHKNUM
102900030721     C                   IF        PiInt=*on
103000030721     C                   Z-ADD     PiVal         VABNCL
103100030721     C                   ELSE
103200030721     C                   SETON                                        32
103300030721     C                   EVAL      VABNCL = *zeros
103400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103500040122     C                             + ' ' + 'VABNCL'
103600030721     C                   ENDIF
103700030721     C                   ENDIF
103800030721     C*** PKB
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
104000030721     C                   EVAL      PiStr=SkSplitCSV(i)
104100030721     C                   EXSR      CHKNUM
104200030721     C                   IF        PiNum=*on
104300030721     C                   Z-ADD     PiVal         VABPKB
104400030721     C                   ELSE
104500030721     C                   SETON                                        32
104600030721     C                   EVAL      VABPKB = *zeros
104700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104800030721     C                             + ' ' + 'VABPKB'
104900030721     C                   ENDIF
105000030721     C                   ENDIF
105100030721     C*** CAS
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
105300080125     C                   IF        SkSplitCSV(i) <> *blanks
105400080125     C                   EVAL      wFlgCAS = '1'
105500080125     C                   ENDIF
105600030721     C                   EVAL      PiStr=SkSplitCSV(i)
105700030721     C                   EXSR      CHKNUM
105800030721     C                   IF        PiNum=*on
105900030721     C                   Z-ADD     PiVal         VABCAS
106000030721     C                   ELSE
106100030721     C                   SETON                                        32
106200030721     C                   EVAL      VABCAS = *zeros
106300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106400030721     C                             + ' ' + 'VABCAS'
106500030721     C                   ENDIF
106600030721     C                   ENDIF
106700030721     C*** CCM
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
106900030721     C                   EVAL      PiStr=SkSplitCSV(i)
107000030721     C                   EXSR      CHKNUM
107100030721     C                   IF        PiInt=*on      AND
107200030721     C                             PiVal<=9999999 AND
107300030721     C                             PiVal>*zeros
107400030721     C                   Z-ADD     PiVal         VABCCM
107500030721     C                   ELSE
107600030721     C                   SETON                                        32
107700030721     C                   EVAL      VABCCM = *zeros
107800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107900040510     C                             + ' ' + 'VABCCM'
108000030721     C                   ENDIF
108100030721     C                   ENDIF
108200030721     C*** LNP
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
108400030721     C                   EVAL      PiStr=SkSplitCSV(i)
108500030721     C                   EXSR      CHKNUM
108600030721     C                   IF        PiInt=*on    AND
108700030721     C                             PiVal<=999   AND
108800030721     C                             PiVal>*zeros
108900030721     C                   Z-ADD     PiVal         VABLNP
109000030721     C                   ELSE
109100040715     C                   SETON                                        32
109200030721     C                   EVAL      VABLNP = *zeros
109300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109400040510     C                             + ' ' + 'VABLNP'
109500030721     C                   ENDIF
109600030721     C                   ENDIF
109700030721     C*** NRS
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
109900030721     C                   EVAL      PiStr=SkSplitCSV(i)
110000030721     C                   EXSR      CHKNUM
110100030721     C                   IF        PiInt=*on AND
110200030721     C                             PiVal<=99
110300030721     C                   Z-ADD     PiVal         VABNRS
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110700030721     C                             + ' ' + 'VABNRS'
110800030721     C                   ENDIF
110900030721     C                   ENDIF
111000030721     C*** LNA
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiInt=*on    AND
111500030721     C                             PiVal<=999   AND
111600030721     C                             PiVal>*zeros
111700030721     C                   Z-ADD     PiVal         VABLNA
111800030721     C                   ELSE
111900030721     C                   SETON                                        32
112000030721     C                   EVAL      VABLNA = *zeros
112100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112200030721     C                             + ' ' + 'VABLNA'
112300030721     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** CTR
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
112700030721     C                   EVAL      PiStr=SkSplitCSV(i)
112800030721     C                   EXSR      CHKNUM
112900030721     C                   IF        PiInt=*on  AND
113000030721     C                             PiVal<=999
113100030721     C                   Z-ADD     PiVal         VABCTR
113200030721     C                   ELSE
113300030721     C                   SETON                                        32
113400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113500030721     C                             + ' ' + 'VABCTR'
113600030721     C                   ENDIF
113700030721     C                   ENDIF
113800030721     C*** IAS
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
114000030721     C                   EVAL      PiStr=SkSplitCSV(i)
114100030721     C                   EXSR      CHKNUM
114200030721     C                   IF        PiNum=*on
114300030721     C                   Z-ADD     PiVal         VABIAS
114400030721     C                   ELSE
114500030721     C                   SETON                                        32
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700030721     C                             + ' ' + 'VABIAS'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** VLB
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiNum=*on
115500030721     C                   Z-ADD     PiVal         VABVLB
115600030721     C                   ELSE
115700030721     C                   SETON                                        32
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900030721     C                             + ' ' + 'VABVLB'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200030721     C*** QFT
116300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
116400030721     C                   EVAL      PiStr=SkSplitCSV(i)
116500030721     C                   EXSR      CHKNUM
116600030721     C                   IF        PiNum=*on
116700030721     C                   Z-ADD     PiVal         VABQFT
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100030721     C                             + ' ' + 'VABQFT'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** NCD
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on      AND
117900030721     C                             PiVal<=9999999
118000030721     C                   Z-ADD     PiVal         VABNCD
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABNCD'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** NCA
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on      AND
119200030721     C                             PiVal<=9999999
119300030721     C                   Z-ADD     PiVal         VABNCA
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700030721     C                             + ' ' + 'VABNCA'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030721     C*** ZNC
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiInt=*on AND
120500030721     C                             PiVal<=99
120600030721     C                   Z-ADD     PiVal         VABZNC
120700030721     C                   ELSE
120800030721     C                   SETON                                        32
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABZNC'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** DCR
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on       AND
121800030721     C                             PiVal<=99999999
121900030721     C                   Z-ADD     PiVal         VABDCR
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABDCR'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** HCR
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on   AND
123100030721     C                             PiVal<=9999
123200030721     C                   Z-ADD     PiVal         VABHCR
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABHCR'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** VMD
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiNum=*on
124400030721     C                   Z-ADD     PiVal         VABVMD
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABVMD'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** ANT
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiInt=*on        AND
125600030721     C                             PiVal<=999999999
125700030721     C                   Z-ADD     PiVal         VABANT
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABANT'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** CMO
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiInt=*on
126900030721     C                   Z-ADD     PiVal         Num5_0
127000030721     C                   MOVEL(p)  Num5_0        VABCMO
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      VABCMO = *zeros
127400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127500030721     C                             + ' ' + 'VABCMO'
127600030721     C                   ENDIF
127700030721     C                   ENDIF
127800040510     C*
127900040510     C* Reperisco i campi del FIVAT
128000040510     C*** VATNOT
128100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
128200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
128300040510     C                   ENDIF
128400040510     C*** VATTRC
128500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
128600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
128700040510     C                   ENDIF
128800060331     C*** VATNOT_A
128900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
129000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
129100060331     C                   ENDIF
129200060331     C*** VATNOT_B
129300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
129400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
129500071106     C                   EVAL      VABNOT    = %trim(SkSplitCSV(i))
129600060331     C                   ENDIF
129700060331     C*** VATNOT_E
129800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
129900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
130000060331     C                   ENDIF
130100070823     C*
130200071029     C***  ===>  Gestione campi "particolari" dentro ciclo
130300071204     C*
130400071204     C*** NSP/RMN
130500071204     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
130600071204     C                   EVAL      PiStr=SkSplitCSV(i)
130700071204     C                   EXSR      CHKNUM
130800071204     C                   IF        PiInt=*on
130900071204     C                   Z-ADD     PiVal         VABNSP
131000071204     C                   Z-ADD     PiVal         VATNSP
131100071204     C                   Z-ADD     PiVal         VABRMN
131200071204     C                   ELSE
131300071204     C                   SETON                                        32
131400071204     C                   EVAL      VABNSP = *zeros
131500071204     C                   EVAL      VATNSP = *zeros
131600071204     C                   EVAL      VABRMN = *zeros
131700071204     C                   EVAL      vinmsg = %trimr(vinmsg)
131800071204     C                             + ' ' + 'VABNSP VATNSP VABRMN'
131900071204     C                   ENDIF
132000071204     C                   ENDIF
132100071204     C*
132200071204     C*** PKB
132300071204     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
132400071204     C                   EVAL      PiStr=SkSplitCSV(i)
132500071204     C                   EXSR      CHKNUM
132600071204     C                   IF        PiNum=*on
132700071204     C                   Z-ADD(H)  PiVal         VABPKB
132800071204     C                   ELSE
132900071204     C                   SETON                                        32
133000071204     C                   EVAL      VABPKB = *zeros
133100071204     C                   EVAL      vinmsg = %trimr(vinmsg)
133200071204     C                             + ' ' + 'VABPKB'
133300071204     C                   ENDIF
133400071204     C                   ENDIF
133500070925     C*
133600071029     C***  <===  -----------------------------------------
133700040510     C*
133800030715     C                   ENDIF
133900030715     C                   EVAL      i = i + 1
134000030715     C                   ENDDO
134100070925     C*
134200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
134300080125     C*
134400080125     C* CTR / CTM
134500080125     C                   IF        VABCTR = 353
134600080125     C                   EVAL      VABCTM = '7R'
134700080125     C                   ENDIF
134800070925     C*
134900071029     C***  <===  -----------------------------------------
135000070925     C*
135100070925     C* Considerazioni finali su CBO/CAS
135200080125     C                   IF        wFlgCAS = '1'
135300070925     C                   IF        VABCBO = '1'
135400070925     C                   EVAL      VABCBO = '4'
135500070925     C                   ENDIF
135600070925     C                   IF        VABCBO = '2'
135700070925     C                   EVAL      VABCBO = '6'
135800070925     C                   ENDIF
135900070925     C                   ENDIF
136000070925     C***  <===  ----------------------------
136100040510     C*
136200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
136300040510     C                   IF        VABNSP = *zeros
136400040510     C                   SETON                                        31
136500040510     C                   ENDIF
136600040715     C*
136700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
136800040715     C                   IF        VABLNP = *zeros
136900040715     C                   SETON                                        31
137000040715     C                   ENDIF
137100040510     C*
137200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
137300040510     C                   IF        VABRMN = *zeros
137400040510     C                   Z-ADD     VABNSP        VABRMN
137500040510     C                   ENDIF
137600040510     C*
137700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
137800040510     C                   IF        VABRMA = *blanks
137900040714     C                   MOVEL     VABRMN        VABRMA
138000040510     C                   ENDIF
138100040128     C*
138200040510     C* Se provincia nn valorizzata la reperisco
138300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
138400040510     C                   IF        VABPRD = *blanks
138500040128     C                   CLEAR                   TISI95DS
138600040128     C                   EVAL      I95TCN = '3'
138700040128     C                   Z-ADD     datcor        I95DAT
138800040128     C                   EVAL      I95NAR = VABNZD
138900040128     C                   EVAL      I95CAP = VABCAD
139000040128     C                   EVAL      I95LOC = VABLOD
139100040128     C                   CALL      'TISI95R'
139200040128     C                   PARM                    TISI95DS
139300040128     C                   EVAL      VABPRD = O95PRV
139400040510     C                   ENDIF
139500040113     C*
139600040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
139700040113     C                   IF        VABCAS > *zeros
139800040119     C                   EVAL      VABVCA = 'EUR'
139900040119     C                   IF        VABCBO = '1'
140000040113     C                   EVAL      VABCBO = '4'
140100040119     C                   ENDIF
140200040119     C                   IF        VABCBO = '2'
140300040119     C                   EVAL      VABCBO = '6'
140400040119     C                   ENDIF
140500040113     C                   ENDIF
140600020204     C*
140700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
140800020204     C                   EXSR      CHKIMPDIV
140900030715     C                   ENDIF
141000020204     C*
141100010607     C                   ENDSR
141200010607     C*----------------------------------------------------*
141300010601
141400020204
141500020204     C*----------------------------------------------------*
141600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
141700020204     C*----------------------------------------------------*
141800020204     C     CHKIMPDIV     BEGSR
141900020204     C*
142000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
142100020204     C                   Z-ADD     *zeros        wrkDec            9 9
142200020204     C*
142300020204     C* Come prima cosa effettuo considerazioni sulla divisa
142400020204     C                   IF        vabIAS > *zeros
142500020204     C                   IF        vabVAS <> 'EUR'
142600020204     C                   EVAL      vabVAS =  'ITL'
142700020204     C                   ENDIF
142800020204     C                   ENDIF
142900020204     C*
143000020204     C                   IF        vabCAS > *zeros
143100020204     C                   IF        vabVCA <> 'EUR'
143200020204     C                   EVAL      vabVCA =  'ITL'
143300020204     C                   ENDIF
143400020204     C                   ENDIF
143500020204     C*
143600020204     C                   IF        vabVMD > *zeros
143700020204     C                   IF        vabVAD <> 'EUR'
143800020204     C                   EVAL      vabVAD =  'ITL'
143900020204     C                   ENDIF
144000020204     C                   ENDIF
144100020204     C*
144200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
144300020204     C                   Z-ADD     vabIAS        wrkDec
144400020204     C                   IF        wrkDec > *zeros
144500020204     C                   IF        vabVAS = 'ITL'
144600020204     C                   EVAL      vabIAS = *zeros
144700020204     C                   ENDIF
144800020204     C                   ENDIF
144900020204     C*
145000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
145100020204     C                   Z-ADD     vabCAS        wrkDec
145200020204     C                   IF        wrkDec > *zeros
145300020204     C                   IF        vabVCA = 'ITL'
145400020204     C                   EVAL      vabCAS = *zeros
145500020204     C                   ENDIF
145600020204     C                   ENDIF
145700020204     C*
145800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
145900020204     C                   Z-ADD     vabVMD        wrkDec
146000020204     C                   IF        wrkDec > *zeros
146100020204     C                   IF        vabVAD = 'ITL'
146200020204     C                   EVAL      vabVMD = *zeros
146300020204     C                   ENDIF
146400020204     C                   ENDIF
146500020204     C*
146600020204     C                   ENDSR
146700020204     C***
146800020204
146900010330
147000010330
147100010330     C*----------------------------------------------------*
147200010330     C*  CONTROLLO NUMERICITA' CAMPI
147300010330     C*----------------------------------------------------*
147400010330     C     CHKNUM        BEGSR
147500010330     C*
147600010606     C                   IF        PiDecChr = *blanks
147700030715     C                   EVAL      PiDecChr = CharNUM
147800010606     C                   ENDIF
147900010606     C*
148000010606     C                   CALL(e)   'ISNUMERIC'
148100010330     C                   PARM                    PiStr            30
148200010606     C                   PARM                    PiDecChr          1
148300010330     C                   PARM      *ZEROS        PiVal            30 9
148400010330     C                   PARM      '0'           PiInt             1
148500010330     C                   PARM      '0'           PiNum             1
148600010330     C                   IF        %error
148700010606     C                   EVAL      PiNum=*off
148800010330     C                   ENDIF
148900010330     C*
149000010330     C                   ENDSR
149100010330     C***
149200010330
149300010601
149400010601
149500010601
149600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149700010601     C     repfil        BEGSR
149800010601     C*
149900010601     C                   if        invfil = *zeros and
150000010601     C                             depfil > *zeros and
150100010629     C                             (vinflg = *blanks or
150200010629     C                              vinflg = *zeros)
150300010601     C
150400010601     C                   eval      invfil = depfil
150500010601     C                   endif
150600010601     C*
150700010601     C                   if        depfil <> invfil and
150800010601     C                             invfil > *zeros
150900010601     C                   eval      flgMulti = '1'
151000010601     C                   if        vinflg = *blanks
151100010601     C                   add       1             cntNonEl
151200010601     C                   endif
151300010601     C                   endif
151400010601     C*
151500010601     C                   if        vinflg = '2'
151600010601     C                   eval      flgStato = '2'
151700010601     C                   endif
151800010601     C*
151900010601     C                   ENDSR
152000010601     C***
152100010601
152200010601
152300010601
152400010330
152500040119      /TITLE Invio dei dati al punto operativo.
152600040119     C     invio         BEGSR
152700040119     C*
152800040119     C* 1° invio FIVAT
152900040119     C                   reset                   dscmz
153000040119     C                   move      vlrpoi        cmzdst
153100040119     C                   eval      cmzfld = 'FIVATWWR'
153200040119     C                   eval      cmzmbd = vlrhdl
153300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
153400040119     C***                if        prmfir = *blanks
153500040119     C                   eval      cmzfla = 'FIVAT00F'
153600040119     C                   eval      cmzmba = 'FIVAT00F'
153700040119     C***                else
153800040119     C***                eval      cmzfla = prmfir
153900040119     C***                eval      cmzmba = prmfir
154000040119     C***                endif
154100040119     C                   eval      cmznrr = *zeros
154200040119     C                   move      §ctrokvt      cmznrr
154300040119     C                   eval      cmzlba = vlrfl1
154400040119     C                   call(e)   'TIS711C'
154500040119     C                   parm                    dscmz
154600040119     C                   parm      *blanks       esito
154700040119     C                   if        %error
154800040119     C                             or cmzerr = '1'
154900040119     C                             or esito  = '1'
155000040119     C                   eval      wrkesito = '3'
155100040119     C                   else
155200040119     C*
155300040119     C* 2° invio FIVAB
155400040119     C                   reset                   dscmz
155500040119     C                   move      vlrpoi        cmzdst
155600040119     C                   eval      cmzfld = vlrfou
155700040119     C                   eval      cmzmbd = vlrhdl
155800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
155900040119     C***                if        prmfir = *blanks
156000040119     C                   eval      cmzfla = 'FIVAB00F'
156100040119     C                   eval      cmzmba = 'FIVAB00F'
156200040119     C***                else
156300040119     C***                eval      cmzfla = prmfir
156400040119     C***                eval      cmzmba = prmfir
156500040119     C***                endif
156600040119     C                   eval      cmznrr = *zeros
156700040119     C                   move      §ctrokvb      cmznrr
156800040119     C                   eval      cmzlba = vlrfl1
156900040119     C                   call(e)   'TIS711C'
157000040119     C                   parm                    dscmz
157100040119     C                   parm      *blanks       esito
157200040119     C                   if        %error
157300040119     C                             or cmzerr = '1'
157400040119     C                             or esito  = '1'
157500040119     C                   eval      wrkesito = '3'
157600040119     C                   endif
157700040119     C                   endif
157800040119     C*
157900040119     C                   ENDSR
158000040119     C***
158100010601
158200010601
158300010601
158400010601
158500010601      /TITLE Invio dei dati al punto operativo.
158600010601     C     opeini        BEGSR
158700010601     C*
158800010601     C* Inizializzo flag e contatori operativi
158900010601     C                   movel     '0'           flgGiro           1
159000010601     C                   movel     '0'           flgMulti          1
159100010601     C                   movel     '1'           flgStato          1
159200010615     C                   movel     '0'           flgOk             1
159300010601     C                   z-add     *zeros        cntNonEl         10 0
159400010601     C                   z-add     *zeros        depfil            3 0
159500010601     C                   z-add     *zeros        invfil            3 0
159600010601     C*
159700010601     C                   ENDSR
159800010601     C***
159900070326
160000070326
160100070326
160200070326
160300070326     C     *pssr         BEGSR
160400070329     C*
160500070329     C                   if        %open(tivin00r)
160600070329     C                   close     tivin00r
160700070329     C                   endif
160800070329     C                   if        %open(fivabwwr)
160900070329     C                   close     fivabwwr
161000070329     C                   endif
161100070329     C                   if        %open(fivatwwr)
161200070329     C                   close     fivatwwr
161300070329     C                   endif
161400070326     C*
161500070326     C* Effettuo la chiamata al CLLE preposto
161600070326     C                   call(e)   'TITVVTC'
161700070326     C                   parm                    parccm
161800070326     C                   parm                    parmbr
161900070326     C                   parm      '2'           paropz
162000070326     C*
162100070326     C                   eval      wrkesito = '2'
162200070404     C*
162300070404     C                   seton                                        LR
162400070326     C*
162500070326     C                   ENDSR     '*CANCL'
162600070326     C***
162700070326
162800070326
162900010330
163000010330
163100000613     C     *inzsr        BEGSR
163200990910     C*
163300990910     C     *entry        plist
163400990920     C                   parm                    tivlrds
163500990921     C                   parm      wrkesito      esito
163600000724     C                   parm                    prmlit
163700000710     C                   parm                    prmfir
163800010330     C*
163900010330     C* CALCOLA LA DATA CORRENTE
164000010330     C                   time                    wn14             14 0
164100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
164200010330     C                   z-add     wn8           g08dat
164300010330     C                   z-add     *zeros        g08inv
164400010330     C                   movel     '0'           g08err
164500010330     C                   call      'XSRDA8'
164600010330     C                   parm                    wlbda8
164700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
164800000613     C*
164900000613     C                   ENDSR
165000000613     C***
165100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
165200080317VABNSP;VABCBO;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABNZD;VABCTR;
165300080206VABCAS;VABPKB;FILL02;VABIAS;VATNOT_E;VABNOT;VABNT2;VABRD2
165400080125
