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
002500080219     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
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
014900080404     C                   exsr      inzvar
015000080404     C                   exsr      impvab
015100080325     C*
015200080325     C* Accendo l'indicatore x forzatura scrittura buffers out anche se no num. sped. (vedi Disk A)
015300080325     C                   seton                                        33
015400010330     C*
015500030822     C                   do        *HIVAL
015600010330     C*
015700030822     C                   read      tivin00r                               70
015800010618     C*
015900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016000010618     C                   if        vinflg = '1'
016100010618     C                   eval      flgOk = '1'
016200010618     C                   endif
016300040510     C*
016400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016500040510     C                   z-add     *zeros        wLenVINDTA        4 0
016600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
016800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
017000040510     C                   z-add     1             wFlgVINDTA
017100040510     C                   leave
017200040510     C                   endif
017300040510     C                   enddo
017400010618     C*
017500040510     C                   if        vindta > *blanks AND
017600040510     C                             wFlgVINDTA = 1
017700010330     C                   add       1             rrnum
017800010330     C*
017900010601     C                   if        *in70 = *off and
018000010330     C                             (vinflg = *blanks
018100010330     C                              or vinflg = '0'
018200010330     C                              or vinflg = '2')
018300010330     C*
018400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018500010711     C                   if        vinflg = *blanks or vinflg = '0'
018600010711     C                   clear                   vinmsg
018700010711     C                   endif
018800010601     C*
018900070925     C* Aggiungo un byte a blanks all'inizio d ogni record
019000070925     C                   eval      vindta  = ' ' + %trim(vindta)
019100070925     C*
019200010330     C                   exsr      inzvar
019300010605     C                   exsr      impvab
019400010601     C*
019500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019600010605     C                   eval      depfil = VABLNP
019700010601     C                   exsr      repfil
019800010601     C                   if        depfil = invfil
019900021025     C                   if        vlrpoi = 999
020000030822     C                   move(P)   invfil        VABFGS
020100021025     C                   else
020200030822     C                   move(P)   vlrpoi        VABFGS
020300021025     C                   endif
020400010601     C*
020500010601     C                   exsr      PREELA
020600010601     C*
020700010604     C* Ebbene...
020800010604     C*
020900040119     C  N31              add       1             §CTROKVB
021000040119     C   32              add       1             §CTRMO
021100040119     C   31              add       1             §CTRNO
021200030822     C                   if        wGiro = 2
021300040510     C* Scrivo sempre il VAT
021400040510     C  N31              exsr      wrivat
021500040510     C* Scrivo il VAB solo a rottura d spedizione
021600040119     C                   if        depspe =  *zeros OR
021700040119     C                             depspe <> vabNSP
021800040119     C  N31              exsr      wrivab
021900080404     C  N31              eval      depspe = vabNSP
022000040119     C                   endif
022100040119     C                   endif
022200010604     C*
022300010604     C                   if        *in31 = *off and
022400010604     C                             *in32 = *off
022500010604     C                   eval      vinflg = '1'
022600010604     C                   else
022700010604     C                   eval      vinflg = '2'
022800010604     C                   endif
022900010604     C                   endif
023000010601     C*
023100010604     C                   endif
023200010604     C*
023300010330     C                   else
023400010330     C                   eval      vinflg = '1'
023500010330     C                   endif
023600010601     C*
023700010601     C  N70              update    tivin000
023800010330     C*
023900030822     C  N70              enddo
024000040119     C*
024100040119     C                   endif                                                  (endif - chkcall)
024200010601     C*
024300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024400010601     C                   if        cntNonEl = *zeros or
024500010601     C                             flgMulti = '0'
024600010330     C* Se non ci sono record con errori ...
024700010601     C                   if        §ctrno = 0 and
024800010604     C                             §ctrmo = 0 and
024900010601     C                             flgStato <> '2'
025000010330     C* ... restituisco esito OK.
025100010330     C                   eval      wrkesito = '0'
025200010330     C                   else
025300040119     C                   if        §ctrokvb > 0
025400010330     C                   eval      wrkesito = '1'
025500010330     C                   else
025600010615     C                   if        flgOk = '0'
025700010615     C                   eval      wrkesito = '2'
025800010615     C                   else
025900010615     C                   eval      wrkesito = '6'
026000010615     C                   endif
026100010330     C                   endif
026200010330     C                   endif
026300010601     C                   else
026400010601     C                   eval      wrkesito = '9'
026500010601     C                   endif
026600010330     C*
026700010330     C                   if        %open(tivin00r)
026800010330     C                   close     tivin00r
026900010330     C                   endif
027000021025     C                   if        %open(fivabwwr)
027100021025     C                   close     fivabwwr
027200010330     C                   endif
027300040512     C                   if        %open(fivatwwr)
027400040512     C                   close     fivatwwr
027500040512     C                   endif
027600010601     C*
027700010601     C                   if        vlrpoi <> 999
027800010601     C                   eval      invfil = vlrpoi
027900010601     C                   endif
028000010330     C*
028100040119     C                   if        §ctrokvb > 0
028200010601     C                             and invfil > *zeros
028300010330     C                   exsr      invio
028400010330     C                   endif
028500010601     C*
028600010618     C                   if        flgGiro = '1'
028700010601     C                   exsr      endela
028800010618     C                   endif
028900010330     C*
029000010330     C                   ENDSR
029100010330     C***
029200040119
029300040119
029400040119
029500040119     C*----------------------------------------------------*
029600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
029700040119     C*----------------------------------------------------*
029800040510     C     WRIVAB        BEGSR
029900040119     C*
030000060331     C* Gestisco l'eventuale rottura x numero spedizione
030100080325     C                   if        VABNSP <> depspe OR *in33 = *on
030200070301     C                   if        wVATNOT_E <> *blanks
030300070301     C                   eval      VABCTM = '7Q'
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
039900030822     C*
040000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
040100030822     C                   CLEAR                   FIVAB000
040200040119     C                   CLEAR                   FIVAT000
040300030822     C                   CLEAR                   SkSplitCSV
040400030822     C                   EVAL      i = 1
040500030822     C                   EVAL      posDa = *zeros
040600030822     C                   EVAL      posA  = *zeros
040700030822     C*
040800030822     C* Reimposto i valori di default
040900030822     C                   EXSR      DEFCAM
041000030822     C*
041100010330     C                   ENDSR
041200010330     C*----------------------------------------------------*
041300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
041400010330     C*----------------------------------------------------*
041500010330     C     DEFCAM        BEGSR
041600080701     C*
041700080701     C* Eseguo forzatura default "extra-parametri"
041800080701     C                   EVAL      VABTIC = 'BM'
041900030822     C*
042000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
042100030715     C* e delimitatore testo.
042200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
042300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
042400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
042500030822     C*
042600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
042700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
042800030715     C                   SETON                                        50
042900030715     C                   ELSE
043000030715     C                   SETOFF                                       50
043100030715     C                   ENDIF
043200030822     C*
043300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
043400030822     C                   EVAL      posDaDft = 1
043500030822     C                   EVAL      posADft  = 0
043600030822     C                   EVAL      wGiroDft = 0
043700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
043800030822     C                             posDaDft > 0
043900030822     C*
044000030822     C* Gestisco il 1° giro
044100030822     C                   IF        wGiroDft = 0
044200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
044300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
044400030822     C* Incremento il contatore dei "giri"
044500030822     C                   EVAL      wGiroDft = 1
044600030822     C                   ELSE
044700030822     C                   EVAL      posDaDft = posADft
044800030822     C                   ENDIF
044900030822     C* Eseguo lo scan x trovare la fine del campo corrente
045000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
045100030822     C*
045200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
045300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
045400030822     C                   IF        posDaDft > 0 AND
045500030822     C                             posADft  > 0
045600030822     C* NCL
045700030822     C                   IF        %subst(
045800030822     C                             %subst(vlrppt:posDaDft+1:
045900030822     C                             posADft-posDaDft-1):1:3)
046000030822     C                             = 'NCL'
046100030822     C                   EVAL      PiStr=%trim(%subst(
046200030822     C                             %subst(vlrppt:posDaDft+1:
046300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046400030822     C                   EXSR      CHKNUM
046500030822     C                   IF        PiInt=*on
046600030822     C                   Z-ADD     PiVal         VABNCL
046700030822     C                   ENDIF
046800030822     C                   ENDIF
046900030822     C* CCM
047000030822     C                   IF        %subst(
047100030822     C                             %subst(vlrppt:posDaDft+1:
047200030822     C                             posADft-posDaDft-1):1:3)
047300030822     C                             = 'CCM'
047400030822     C                   EVAL      PiStr=%trim(%subst(
047500030822     C                             %subst(vlrppt:posDaDft+1:
047600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047700030822     C                   EXSR      CHKNUM
047800030822     C                   IF        PiInt=*on
047900030822     C                   Z-ADD     PiVal         VABCCM
048000030822     C                   ENDIF
048100030822     C                   ENDIF
048200030822     C* LNP
048300030822     C                   IF        %subst(
048400030822     C                             %subst(vlrppt:posDaDft+1:
048500030822     C                             posADft-posDaDft-1):1:3)
048600030822     C                             = 'LNP'
048700030822     C                   EVAL      PiStr=%trim(%subst(
048800030822     C                             %subst(vlrppt:posDaDft+1:
048900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049000030822     C                   EXSR      CHKNUM
049100030822     C                   IF        PiInt=*on
049200030822     C                   Z-ADD     PiVal         VABLNP
049300030822     C                   ENDIF
049400030822     C                   ENDIF
049500030822     C* NRS
049600030822     C                   IF        %subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):1:3)
049900030822     C                             = 'NRS'
050000030822     C                   EVAL      PiStr=%trim(%subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050300030822     C                   EXSR      CHKNUM
050400030822     C                   IF        PiInt=*on
050500030822     C                   Z-ADD     PiVal         VABNRS
050600030822     C                   ENDIF
050700030822     C                   ENDIF
050800030822     C* CTR
050900030822     C                   IF        %subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):1:3)
051200030822     C                             = 'CTR'
051300030822     C                   EVAL      PiStr=%trim(%subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051600030822     C                   EXSR      CHKNUM
051700030822     C                   IF        PiInt=*on
051800030822     C                   Z-ADD     PiVal         VABCTR
051900030822     C                   ENDIF
052000030822     C                   ENDIF
052100030822     C* PKB
052200030822     C                   IF        %subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):1:3)
052500030822     C                             = 'PKB'
052600030822     C                   EVAL      PiStr=%trim(%subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052900030822     C                   EXSR      CHKNUM
053000030822     C                   IF        PiNum=*on
053100030822     C                   Z-ADD     PiVal         VABPKB
053200030822     C                   ENDIF
053300030822     C                   ENDIF
053400030822     C* VLB
053500030822     C                   IF        %subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):1:3)
053800030822     C                             = 'VLB'
053900030822     C                   EVAL      PiStr=%trim(%subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200030822     C                   EXSR      CHKNUM
054300030822     C                   IF        PiNum=*on
054400030822     C                   Z-ADD     PiVal         VABVLB
054500030822     C                   ENDIF
054600030822     C                   ENDIF
054700030822     C* QFT
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'QFT'
055200030822     C                   EVAL      PiStr=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   EXSR      CHKNUM
055600030822     C                   IF        PiNum=*on
055700030822     C                   Z-ADD     PiVal         VABQFT
055800030822     C                   ENDIF
055900030822     C                   ENDIF
056000030822     C* CBO
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'CBO'
056500030822     C                   EVAL      VABCBO=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   ENDIF
056900030822     C* TSP
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'TSP'
057400030822     C                   EVAL      VABTSP=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   ENDIF
057800030822     C* VAS
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'VAS'
058300030822     C                   EVAL      VABVAS=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   ENDIF
058700030822     C* VCA
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'VCA'
059200030822     C                   EVAL      VABVCA=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   ENDIF
059600070925     C* TIC
059700070925     C                   IF        %subst(
059800070925     C                             %subst(vlrppt:posDaDft+1:
059900070925     C                             posADft-posDaDft-1):1:3)
060000070925     C                             = 'TIC'
060100070925     C                   EVAL      VABTIC=%trim(%subst(
060200070925     C                             %subst(vlrppt:posDaDft+1:
060300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400070925     C                   ENDIF
060500030822     C* GCA
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900030822     C                             = 'GCA'
061000030822     C                   EVAL      VABGCA=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   ENDIF
061400030822     C* CTM
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'CTM'
061900030822     C                   EVAL      VABCTM=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   ENDIF
062300030822     C* FFD
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'FFD'
062800030822     C                   EVAL      VABFFD=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   ENDIF
063200030822     C* VAD
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'VAD'
063700030822     C                   EVAL      VABVAD=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   ENDIF
064100030822     C* GMA
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'GMA'
064600030822     C                   EVAL      VABGMA=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   ENDIF
065000030822     C* GGA
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'GGA'
065500030822     C                   EVAL      VABGGA=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* GVA
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'GVA'
066400030822     C                   EVAL      VABGVA=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800030822     C* TC1
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'TC1'
067300030822     C                   EVAL      VABTC1=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   ENDIF
067700030822     C* TC2
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'TC2'
068200030822     C                   EVAL      VABTC2=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   ENDIF
068600040714     C* VATTRC
068700040714     C                   IF        %subst(
068800040714     C                             %subst(vlrppt:posDaDft+1:
068900040714     C                             posADft-posDaDft-1):1:3)
069000040714     C                             = 'TRC'
069100040714     C                   EVAL      VATTRC=%trim(%subst(
069200040714     C                             %subst(vlrppt:posDaDft+1:
069300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400040714     C                   ENDIF
069500030822     C* ...
069600030822     C                   ENDIF
069700030822     C                   ENDDO
069800020204     C*
069900010330     C                   ENDSR
070000010607     C*----------------------------------------------------*
070100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
070200010607     C*----------------------------------------------------*
070300010607     C     IMPVAB        BEGSR
070400010607     C*
070500010607     C                   SETOFF                                       3132
070600030515     C*
070700010607     C                   MOVEL     datcor        VABAAS
070800010607     C                   MOVE      datcor        VABMGS
070900010607     C*
071000030715     C**********
071100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
071200030715     C**********
071300030715     C                   IF        wGiro = *zeros
071400040113     C*
071500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
071600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
071700040510     C                             %trim(intcol(3))<>*blanks
071800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
071900040113     C                                      %trim(intcol(3))
072000070823     C     ';':CharCSV   XLATE     vindta        vindta
072100040510     C                   ENDIF
072200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
072300031201     C     minu:maiu     XLATE     vindta        vindta
072400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
072500030715     C                   DOW       posDa <= %len(%trim(vindta))
072600030715     C*
072700030715     C* Gestisco il 1° campo
072800030715     C                   IF        i = 1
072900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
073000030715     C                   EVAL      posDa = 1
073100030715     C* Eseguo lo scan x trovare la fine del primo campo
073200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
073300030715     C* A questo punto "estraggo" il campo corrente
073400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073500030715     C                                                    (posA-posDa))
073600030715     C* X i campi successivi al 1°
073700030715     C                   ELSE
073800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
073900030715     C                   EVAL      posDa = posA + 1
074000030715     C* Eseguo lo scan x trovare la fine del campo corrente
074100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074200030715     C* Gestisco l'ultimo campo
074300030715     C                   IF        posA = *zeros
074400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
074500030715     C* A questo punto "estraggo" il campo corrente
074600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
074700030715     C                                                    (posA-posDa))
074800030715     C                   LEAVE
074900030715     C                   ELSE
075000030715     C* A questo punto "estraggo" il campo corrente
075100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075200030715     C                                                    (posA-posDa))
075300030715     C                   ENDIF
075400030715     C                   ENDIF
075500030715     C* Incremento il contatore d campo
075600030715     C                   EVAL      i = i +1
075700030715     C                   ENDDO
075800030715     C*
075900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
076000030715     C* delimitazione testo
076100030715     C                   EVAL      i = 1
076200030715     C                   DOW       i < %elem(SkSplitFLD)
076300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
076400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
076500030715     C                   EVAL      i = i + 1
076600030715     C                   ENDDO
076700030715     C                   EVAL      wGiro = 1
076800030715     C                   ELSE
076900030715     C                   EVAL      wGiro = 2
077000031204     C**********
077100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
077200031204     C* d separatore campo
077300031204     C**********
077400031204     C                   EVAL      vindta = %trim(vindta)
077500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
077600031204     C                   EVAL      vindta = %subst(vindta:2)
077700031204     C                   ENDDO
077800031204     C*
077900031204     C                   Z-ADD     *zeros        lunghInput        4 0
078000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
078100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
078200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
078300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
078400031204     C                   ENDDO
078500030715     C**********
078600030715     C* Effettuo lo split del campo dati d input
078700030715     C**********
078800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078900030715     C                   DOW       posDa <= %len(%trim(vindta))
079000030715     C*
079100030715     C* Gestisco il 1° campo
079200030715     C                   IF        i = 1
079300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079400030715     C                   EVAL      posDa = 1
079500030715     C* Eseguo lo scan x trovare la fine del primo campo
079600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079700030715     C* A questo punto "estraggo" il campo corrente
079800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079900030715     C                                                    (posA-posDa))
080000030715     C* X i campi successivi al 1°
080100030715     C                   ELSE
080200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080300030715     C                   EVAL      posDa = posA + 1
080400031204     C* Verifico che nn vi sia il campo nn valorizzato
080500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
080600031204     C* Se campo nn valorizzato skippo al prossimo
080700031204     C                   EVAL      SkSplitCSV(i) = *blanks
080800031204     C                   EVAL      posA  = posA + 1
080900031204     C                   ELSE
081000030715     C* Eseguo lo scan x trovare la fine del campo corrente
081100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081200030715     C* Gestisco l'ultimo campo
081300030715     C                   IF        posA = *zeros
081400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081500030715     C* A questo punto "estraggo" il campo corrente
081600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081700030715     C                                                    (posA-posDa))
081800030715     C                   LEAVE
081900030715     C                   ELSE
082000030715     C* A questo punto "estraggo" il campo corrente
082100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082200030715     C                                                    (posA-posDa))
082300030715     C                   ENDIF
082400031204     C                   ENDIF
082500030715     C                   ENDIF
082600030715     C* Incremento il contatore d campo
082700030715     C                   EVAL      i = i +1
082800030715     C                   ENDDO
082900030715     C*
083000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083100030715     C* delimitazione testo
083200030715     C                   EVAL      i = 1
083300030715     C                   DOW       i < %elem(SkSplitCSV)
083400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
083500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
083600030715     C                   EVAL      i = i + 1
083700030715     C                   ENDDO
083800030715     C*
083900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
084000030715     C                   EVAL      i = 1
084100030715     C                   DOW       i < %elem(SkSplitFLD)
084200030715     C*
084300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
084400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
084500030715     C                             SkSplitCSV(i) <> *blanks
084600030721     C*** ATB
084700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
084800030721     C                   EVAL      VABATB = SkSplitCSV(i)
084900030721     C                   ENDIF
085000030715     C*** RSD
085100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
085200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
085300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
085400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
085500030715     C* ==
085600030715     C                   ENDIF
085700030721     C*** RD2
085800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
085900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
086000030721     C                   ENDIF
086100030715     C*** IND
086200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
086300030715     C                   EVAL      VABIND = SkSplitCSV(i)
086400030715     C                   ENDIF
086500030715     C*** LOD
086600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
086700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
086800030715     C                   ENDIF
086900030715     C*** PRD
087000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
087100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
087200030715     C                   ENDIF
087300030721     C*** NZD
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
087500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030715     C*** RMA
087800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
087900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
088000030715     C                   ENDIF
088100030715     C*** NOT
088200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
088300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
088400030715     C                   ENDIF
088500030715     C*** NT2
088600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
088700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
088800030715     C                   ENDIF
088900030715     C*** VCA
089000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
089100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
089200030715     C                   ENDIF
089300030715     C*** VAS
089400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
089500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
089600030715     C                   ENDIF
089700030715     C*** FFD
089800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
089900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
090000030715     C                   ENDIF
090100030721     C*** GC1
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
090300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030721     C*** GC2
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
090700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
090800030721     C                   ENDIF
090900030721     C*** TSP
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
091100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
091200030721     C                   ENDIF
091300040510     C*** CBO
091400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
091500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
091600040510     C                   ENDIF
091700030721     C*** NAS
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
091900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030721     C*** TIC
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
092300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
092400030721     C                   ENDIF
092500030721     C*** GCA
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
092700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030721     C*** XCO
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
093100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
093200030721     C                   ENDIF
093300030721     C*** CTM
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
093500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
093600030721     C                   ENDIF
093700030721     C*** TCR
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
093900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030721     C*** CTS
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
094300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030721     C*** FTM
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
094700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030721     C*** VAD
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
095100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
095200030721     C                   ENDIF
095300030721     C*** GMA
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
095500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030721     C*** GGA
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
095900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030721     C*** GVA
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
096300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030721     C*** TC1
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
096700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030721     C*** TC2
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
097100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
097200030721     C                   ENDIF
097300030721     C*** SCL
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
097500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030721     C*** RMO
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
097900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030721     C*** NMO
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
098300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*
098600030721     C* Reperisco quindi i campi numerici...
098700040510     C*** NSP
098800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
098900040510     C                   EVAL      PiStr=SkSplitCSV(i)
099000040510     C                   EXSR      CHKNUM
099100040510     C                   IF        PiInt=*on
099200040510     C                   Z-ADD     PiVal         VABNSP
099300040510     C                   ELSE
099400040510     C                   SETON                                        31
099500040510     C                   EVAL      VABNSP = *zeros
099600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
099700040510     C                             + ' ' + 'VABNSP'
099800040510     C                   ENDIF
099900040510     C                   ENDIF
100000071106     C*** RMN
100100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
100200071106     C                   EVAL      PiStr=SkSplitCSV(i)
100300071106     C                   EXSR      CHKNUM
100400071106     C                   IF        PiInt=*on
100500071106     C                   Z-ADD     PiVal         VABRMN
100600071106     C                   ELSE
100700071106     C                   SETON                                        32
100800071106     C                   EVAL      VABRMN = *zeros
100900071106     C                   EVAL      vinmsg = %trimr(vinmsg)
101000071106     C                             + ' ' + 'VABRMN'
101100071106     C                   ENDIF
101200071106     C                   ENDIF
101300030721     C*** CAD
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
101500030721     C                   EVAL      PiStr=SkSplitCSV(i)
101600030721     C                   EXSR      CHKNUM
101700030721     C                   IF        PiInt=*on
101800030721     C                   Z-ADD     PiVal         Num5_0
101900030721     C                   MOVEL(p)  Num5_0        VABCAD
102000030721     C                   ELSE
102100030721     C                   SETON                                        32
102200030721     C                   EVAL      VABCAD = *zeros
102300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102400030721     C                             + ' ' + 'VABCAD'
102500030721     C                   ENDIF
102600030721     C                   ENDIF
102700030721     C*** NCL
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
102900030721     C                   EVAL      PiStr=SkSplitCSV(i)
103000030721     C                   EXSR      CHKNUM
103100030721     C                   IF        PiInt=*on
103200030721     C                   Z-ADD     PiVal         VABNCL
103300030721     C                   ELSE
103400030721     C                   SETON                                        32
103500030721     C                   EVAL      VABNCL = *zeros
103600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103700040122     C                             + ' ' + 'VABNCL'
103800030721     C                   ENDIF
103900030721     C                   ENDIF
104000030721     C*** PKB
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
104200030721     C                   EVAL      PiStr=SkSplitCSV(i)
104300030721     C                   EXSR      CHKNUM
104400030721     C                   IF        PiNum=*on
104500030721     C                   Z-ADD     PiVal         VABPKB
104600030721     C                   ELSE
104700030721     C                   SETON                                        32
104800030721     C                   EVAL      VABPKB = *zeros
104900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105000030721     C                             + ' ' + 'VABPKB'
105100030721     C                   ENDIF
105200030721     C                   ENDIF
105300030721     C*** CAS
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
105500030721     C                   EVAL      PiStr=SkSplitCSV(i)
105600030721     C                   EXSR      CHKNUM
105700030721     C                   IF        PiNum=*on
105800030721     C                   Z-ADD     PiVal         VABCAS
105900030721     C                   ELSE
106000030721     C                   SETON                                        32
106100030721     C                   EVAL      VABCAS = *zeros
106200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106300030721     C                             + ' ' + 'VABCAS'
106400030721     C                   ENDIF
106500030721     C                   ENDIF
106600030721     C*** CCM
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
106800030721     C                   EVAL      PiStr=SkSplitCSV(i)
106900030721     C                   EXSR      CHKNUM
107000030721     C                   IF        PiInt=*on      AND
107100030721     C                             PiVal<=9999999 AND
107200030721     C                             PiVal>*zeros
107300030721     C                   Z-ADD     PiVal         VABCCM
107400030721     C                   ELSE
107500030721     C                   SETON                                        32
107600030721     C                   EVAL      VABCCM = *zeros
107700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107800040510     C                             + ' ' + 'VABCCM'
107900030721     C                   ENDIF
108000030721     C                   ENDIF
108100030721     C*** LNP
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
108300030721     C                   EVAL      PiStr=SkSplitCSV(i)
108400030721     C                   EXSR      CHKNUM
108500030721     C                   IF        PiInt=*on    AND
108600030721     C                             PiVal<=999   AND
108700030721     C                             PiVal>*zeros
108800030721     C                   Z-ADD     PiVal         VABLNP
108900030721     C                   ELSE
109000040715     C                   SETON                                        32
109100030721     C                   EVAL      VABLNP = *zeros
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300040510     C                             + ' ' + 'VABLNP'
109400030721     C                   ENDIF
109500030721     C                   ENDIF
109600030721     C*** NRS
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
109800030721     C                   EVAL      PiStr=SkSplitCSV(i)
109900030721     C                   EXSR      CHKNUM
110000030721     C                   IF        PiInt=*on AND
110100030721     C                             PiVal<=99
110200030721     C                   Z-ADD     PiVal         VABNRS
110300030721     C                   ELSE
110400030721     C                   SETON                                        32
110500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110600030721     C                             + ' ' + 'VABNRS'
110700030721     C                   ENDIF
110800030721     C                   ENDIF
110900030721     C*** LNA
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
111100030721     C                   EVAL      PiStr=SkSplitCSV(i)
111200030721     C                   EXSR      CHKNUM
111300030721     C                   IF        PiInt=*on    AND
111400030721     C                             PiVal<=999   AND
111500030721     C                             PiVal>*zeros
111600030721     C                   Z-ADD     PiVal         VABLNA
111700030721     C                   ELSE
111800030721     C                   SETON                                        32
111900030721     C                   EVAL      VABLNA = *zeros
112000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112100030721     C                             + ' ' + 'VABLNA'
112200030721     C                   ENDIF
112300030721     C                   ENDIF
112400030721     C*** CTR
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiInt=*on  AND
112900030721     C                             PiVal<=999
113000030721     C                   Z-ADD     PiVal         VABCTR
113100030721     C                   ELSE
113200030721     C                   SETON                                        32
113300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113400030721     C                             + ' ' + 'VABCTR'
113500030721     C                   ENDIF
113600030721     C                   ENDIF
113700030721     C*** IAS
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100030721     C                   IF        PiNum=*on
114200030721     C                   Z-ADD     PiVal         VABIAS
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600030721     C                             + ' ' + 'VABIAS'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900030721     C*** VLB
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
115100030721     C                   EVAL      PiStr=SkSplitCSV(i)
115200030721     C                   EXSR      CHKNUM
115300030721     C                   IF        PiNum=*on
115400030721     C                   Z-ADD     PiVal         VABVLB
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABVLB'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** QFT
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiNum=*on
116600030721     C                   Z-ADD     PiVal         VABQFT
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABQFT'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** NCD
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on      AND
117800030721     C                             PiVal<=9999999
117900030721     C                   Z-ADD     PiVal         VABNCD
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300030721     C                             + ' ' + 'VABNCD'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** NCA
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiInt=*on      AND
119100030721     C                             PiVal<=9999999
119200030721     C                   Z-ADD     PiVal         VABNCA
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABNCA'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** ZNC
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on AND
120400030721     C                             PiVal<=99
120500030721     C                   Z-ADD     PiVal         VABZNC
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABZNC'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** DCR
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on       AND
121700030721     C                             PiVal<=99999999
121800030721     C                   Z-ADD     PiVal         VABDCR
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200030721     C                             + ' ' + 'VABDCR'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** HCR
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiInt=*on   AND
123000030721     C                             PiVal<=9999
123100030721     C                   Z-ADD     PiVal         VABHCR
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABHCR'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** VMD
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiNum=*on
124300030721     C                   Z-ADD     PiVal         VABVMD
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABVMD'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** ANT
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on        AND
125500030721     C                             PiVal<=999999999
125600030721     C                   Z-ADD     PiVal         VABANT
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABANT'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** CMO
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiInt=*on
126800030721     C                   Z-ADD     PiVal         Num5_0
126900030721     C                   MOVEL(p)  Num5_0        VABCMO
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      VABCMO = *zeros
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABCMO'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700040510     C*
127800040510     C* Reperisco i campi del FIVAT
127900040510     C*** VATNOT
128000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
128100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
128200040510     C                   ENDIF
128300040510     C*** VATTRC
128400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
128500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
128600040510     C                   ENDIF
128700060331     C*** VATNOT_A
128800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
128900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
129000060331     C                   ENDIF
129100060331     C*** VATNOT_B
129200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
129300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
129400060331     C                   ENDIF
129500060331     C*** VATNOT_E
129600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
129700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
129800060331     C                   ENDIF
129900070823     C*
130000071029     C***  ===>  Gestione campi "particolari" dentro ciclo
130100071204     C*
130200080404     C*** NSP
130300080404     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
130400080404     C                   EVAL      PiStr=SkSplitCSV(i)
130500080404     C                   EXSR      CHKNUM
130600080404     C                   IF        PiInt=*on
130700080404     C                   Z-ADD     PiVal         VABNSP
130800080404     C                   Z-ADD     PiVal         VATNSP
130900080404     C                   Z-ADD     PiVal         VABRMN
131000080404     C                   ELSE
131100080404     C                   SETON                                        31
131200080404     C                   EVAL      VABNSP = *zeros
131300080404     C                   EVAL      vinmsg = %trimr(vinmsg)
131400080404     C                             + ' ' + 'VABNSP VATNSP VABRMN'
131500080404     C                   ENDIF
131600080404     C                   ENDIF
131700080404     C*** CTR
131800080404     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCTR'
131900080404     C                   IF        %trim(SkSplitCSV(i)) = *blanks
132000080404     C                   EVAL      VABCTM = '7Q'
132100080404     C                   EVAL      VABCTR = 000
132200080404     C                   ENDIF
132300080404     C                   IF        %trim(SkSplitCSV(i)) = '1'
132400080404     C                   EVAL      VABCTM = '7R'
132500080404     C                   EVAL      VABCTR = 300
132600080404     C                   ENDIF
132700080404     C                   IF        %trim(SkSplitCSV(i)) = '2'
132800080404     C                   EVAL      VABCTM = '7Q'
132900080404     C                   EVAL      VABCTR = 052
133000080404     C                   ENDIF
133100080404     C                   ENDIF
133200070925     C*
133300071029     C***  <===  -----------------------------------------
133400040510     C*
133500030715     C                   ENDIF
133600030715     C                   EVAL      i = i + 1
133700030715     C                   ENDDO
133800070925     C*
133900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
134000080219     C*
134100080219     C* Considerazioni contrassegno
134200080219     C                   IF        VABCAS = *zeros
134300080219     C                   EVAL      VABTIC = *blanks
134400080219     C                   ENDIF
134500070925     C*
134600071029     C***  <===  -----------------------------------------
134700070925     C*
134800070925     C* Considerazioni finali su CBO/CAS
134900080219     C                   IF        VABCAS > *zeros
135000070925     C                   IF        VABCBO = '1'
135100070925     C                   EVAL      VABCBO = '4'
135200070925     C                   ENDIF
135300070925     C                   IF        VABCBO = '2'
135400070925     C                   EVAL      VABCBO = '6'
135500070925     C                   ENDIF
135600070925     C                   ENDIF
135700040510     C*
135800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
135900040510     C                   IF        VABNSP = *zeros
136000040510     C                   SETON                                        31
136100040510     C                   ENDIF
136200040715     C*
136300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
136400040715     C                   IF        VABLNP = *zeros
136500040715     C                   SETON                                        31
136600040715     C                   ENDIF
136700040510     C*
136800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
136900040510     C                   IF        VABRMN = *zeros
137000040510     C                   Z-ADD     VABNSP        VABRMN
137100040510     C                   ENDIF
137200080225     C*
137300080225     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
137400080225     C                   IF        VABRMA = *blanks
137500080225     C                   MOVE(P)   VABRMN        VABRMA
137600080225     C                   ENDIF
137700040128     C*
137800040510     C* Se provincia nn valorizzata la reperisco
137900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
138000040510     C                   IF        VABPRD = *blanks
138100040128     C                   CLEAR                   TISI95DS
138200040128     C                   EVAL      I95TCN = '3'
138300040128     C                   Z-ADD     datcor        I95DAT
138400040128     C                   EVAL      I95NAR = VABNZD
138500040128     C                   EVAL      I95CAP = VABCAD
138600040128     C                   EVAL      I95LOC = VABLOD
138700040128     C                   CALL      'TISI95R'
138800040128     C                   PARM                    TISI95DS
138900040128     C                   EVAL      VABPRD = O95PRV
139000040510     C                   ENDIF
139100040113     C*
139200040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
139300040113     C                   IF        VABCAS > *zeros
139400040119     C                   EVAL      VABVCA = 'EUR'
139500040119     C                   IF        VABCBO = '1'
139600040113     C                   EVAL      VABCBO = '4'
139700040119     C                   ENDIF
139800040119     C                   IF        VABCBO = '2'
139900040119     C                   EVAL      VABCBO = '6'
140000040119     C                   ENDIF
140100040113     C                   ENDIF
140200020204     C*
140300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
140400020204     C                   EXSR      CHKIMPDIV
140500030715     C                   ENDIF
140600020204     C*
140700010607     C                   ENDSR
140800010607     C*----------------------------------------------------*
140900010601
141000020204
141100020204     C*----------------------------------------------------*
141200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
141300020204     C*----------------------------------------------------*
141400020204     C     CHKIMPDIV     BEGSR
141500020204     C*
141600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
141700020204     C                   Z-ADD     *zeros        wrkDec            9 9
141800020204     C*
141900020204     C* Come prima cosa effettuo considerazioni sulla divisa
142000020204     C                   IF        vabIAS > *zeros
142100020204     C                   IF        vabVAS <> 'EUR'
142200020204     C                   EVAL      vabVAS =  'ITL'
142300020204     C                   ENDIF
142400020204     C                   ENDIF
142500020204     C*
142600020204     C                   IF        vabCAS > *zeros
142700020204     C                   IF        vabVCA <> 'EUR'
142800020204     C                   EVAL      vabVCA =  'ITL'
142900020204     C                   ENDIF
143000020204     C                   ENDIF
143100020204     C*
143200020204     C                   IF        vabVMD > *zeros
143300020204     C                   IF        vabVAD <> 'EUR'
143400020204     C                   EVAL      vabVAD =  'ITL'
143500020204     C                   ENDIF
143600020204     C                   ENDIF
143700020204     C*
143800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
143900020204     C                   Z-ADD     vabIAS        wrkDec
144000020204     C                   IF        wrkDec > *zeros
144100020204     C                   IF        vabVAS = 'ITL'
144200020204     C                   EVAL      vabIAS = *zeros
144300020204     C                   ENDIF
144400020204     C                   ENDIF
144500020204     C*
144600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
144700020204     C                   Z-ADD     vabCAS        wrkDec
144800020204     C                   IF        wrkDec > *zeros
144900020204     C                   IF        vabVCA = 'ITL'
145000020204     C                   EVAL      vabCAS = *zeros
145100020204     C                   ENDIF
145200020204     C                   ENDIF
145300020204     C*
145400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
145500020204     C                   Z-ADD     vabVMD        wrkDec
145600020204     C                   IF        wrkDec > *zeros
145700020204     C                   IF        vabVAD = 'ITL'
145800020204     C                   EVAL      vabVMD = *zeros
145900020204     C                   ENDIF
146000020204     C                   ENDIF
146100020204     C*
146200020204     C                   ENDSR
146300020204     C***
146400020204
146500010330
146600010330
146700010330     C*----------------------------------------------------*
146800010330     C*  CONTROLLO NUMERICITA' CAMPI
146900010330     C*----------------------------------------------------*
147000010330     C     CHKNUM        BEGSR
147100010330     C*
147200010606     C                   IF        PiDecChr = *blanks
147300030715     C                   EVAL      PiDecChr = CharNUM
147400010606     C                   ENDIF
147500010606     C*
147600010606     C                   CALL(e)   'ISNUMERIC'
147700010330     C                   PARM                    PiStr            30
147800010606     C                   PARM                    PiDecChr          1
147900010330     C                   PARM      *ZEROS        PiVal            30 9
148000010330     C                   PARM      '0'           PiInt             1
148100010330     C                   PARM      '0'           PiNum             1
148200010330     C                   IF        %error
148300010606     C                   EVAL      PiNum=*off
148400010330     C                   ENDIF
148500010330     C*
148600010330     C                   ENDSR
148700010330     C***
148800010330
148900010601
149000010601
149100010601
149200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149300010601     C     repfil        BEGSR
149400010601     C*
149500010601     C                   if        invfil = *zeros and
149600010601     C                             depfil > *zeros and
149700010629     C                             (vinflg = *blanks or
149800010629     C                              vinflg = *zeros)
149900010601     C
150000010601     C                   eval      invfil = depfil
150100010601     C                   endif
150200010601     C*
150300010601     C                   if        depfil <> invfil and
150400010601     C                             invfil > *zeros
150500010601     C                   eval      flgMulti = '1'
150600010601     C                   if        vinflg = *blanks
150700010601     C                   add       1             cntNonEl
150800010601     C                   endif
150900010601     C                   endif
151000010601     C*
151100010601     C                   if        vinflg = '2'
151200010601     C                   eval      flgStato = '2'
151300010601     C                   endif
151400010601     C*
151500010601     C                   ENDSR
151600010601     C***
151700010601
151800010601
151900010601
152000010330
152100040119      /TITLE Invio dei dati al punto operativo.
152200040119     C     invio         BEGSR
152300040119     C*
152400040119     C* 1° invio FIVAT
152500040119     C                   reset                   dscmz
152600040119     C                   move      vlrpoi        cmzdst
152700040119     C                   eval      cmzfld = 'FIVATWWR'
152800040119     C                   eval      cmzmbd = vlrhdl
152900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
153000040119     C***                if        prmfir = *blanks
153100040119     C                   eval      cmzfla = 'FIVAT00F'
153200040119     C                   eval      cmzmba = 'FIVAT00F'
153300040119     C***                else
153400040119     C***                eval      cmzfla = prmfir
153500040119     C***                eval      cmzmba = prmfir
153600040119     C***                endif
153700040119     C                   eval      cmznrr = *zeros
153800040119     C                   move      §ctrokvt      cmznrr
153900040119     C                   eval      cmzlba = vlrfl1
154000040119     C                   call(e)   'TIS711C'
154100040119     C                   parm                    dscmz
154200040119     C                   parm      *blanks       esito
154300040119     C                   if        %error
154400040119     C                             or cmzerr = '1'
154500040119     C                             or esito  = '1'
154600040119     C                   eval      wrkesito = '3'
154700040119     C                   else
154800040119     C*
154900040119     C* 2° invio FIVAB
155000040119     C                   reset                   dscmz
155100040119     C                   move      vlrpoi        cmzdst
155200040119     C                   eval      cmzfld = vlrfou
155300040119     C                   eval      cmzmbd = vlrhdl
155400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
155500040119     C***                if        prmfir = *blanks
155600040119     C                   eval      cmzfla = 'FIVAB00F'
155700040119     C                   eval      cmzmba = 'FIVAB00F'
155800040119     C***                else
155900040119     C***                eval      cmzfla = prmfir
156000040119     C***                eval      cmzmba = prmfir
156100040119     C***                endif
156200040119     C                   eval      cmznrr = *zeros
156300040119     C                   move      §ctrokvb      cmznrr
156400040119     C                   eval      cmzlba = vlrfl1
156500040119     C                   call(e)   'TIS711C'
156600040119     C                   parm                    dscmz
156700040119     C                   parm      *blanks       esito
156800040119     C                   if        %error
156900040119     C                             or cmzerr = '1'
157000040119     C                             or esito  = '1'
157100040119     C                   eval      wrkesito = '3'
157200040119     C                   endif
157300040119     C                   endif
157400040119     C*
157500040119     C                   ENDSR
157600040119     C***
157700010601
157800010601
157900010601
158000010601
158100010601      /TITLE Invio dei dati al punto operativo.
158200010601     C     opeini        BEGSR
158300010601     C*
158400010601     C* Inizializzo flag e contatori operativi
158500010601     C                   movel     '0'           flgGiro           1
158600010601     C                   movel     '0'           flgMulti          1
158700010601     C                   movel     '1'           flgStato          1
158800010615     C                   movel     '0'           flgOk             1
158900010601     C                   z-add     *zeros        cntNonEl         10 0
159000010601     C                   z-add     *zeros        depfil            3 0
159100010601     C                   z-add     *zeros        invfil            3 0
159200010601     C*
159300010601     C                   ENDSR
159400010601     C***
159500070326
159600070326
159700070326
159800070326
159900070326     C     *pssr         BEGSR
160000070329     C*
160100070329     C                   if        %open(tivin00r)
160200070329     C                   close     tivin00r
160300070329     C                   endif
160400070329     C                   if        %open(fivabwwr)
160500070329     C                   close     fivabwwr
160600070329     C                   endif
160700070329     C                   if        %open(fivatwwr)
160800070329     C                   close     fivatwwr
160900070329     C                   endif
161000070326     C*
161100070326     C* Effettuo la chiamata al CLLE preposto
161200070326     C                   call(e)   'TITVVTC'
161300070326     C                   parm                    parccm
161400070326     C                   parm                    parmbr
161500070326     C                   parm      '2'           paropz
161600070326     C*
161700070326     C                   eval      wrkesito = '2'
161800070404     C*
161900070404     C                   seton                                        LR
162000070326     C*
162100070326     C                   ENDSR     '*CANCL'
162200070326     C***
162300070326
162400070326
162500010330
162600010330
162700000613     C     *inzsr        BEGSR
162800990910     C*
162900990910     C     *entry        plist
163000990920     C                   parm                    tivlrds
163100990921     C                   parm      wrkesito      esito
163200000724     C                   parm                    prmlit
163300000710     C                   parm                    prmfir
163400010330     C*
163500010330     C* CALCOLA LA DATA CORRENTE
163600010330     C                   time                    wn14             14 0
163700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
163800010330     C                   z-add     wn8           g08dat
163900010330     C                   z-add     *zeros        g08inv
164000010330     C                   movel     '0'           g08err
164100010330     C                   call      'XSRDA8'
164200010330     C                   parm                    wlbda8
164300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
164400000613     C*
164500000613     C                   ENDSR
164600000613     C***
164700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
164800080404VATNOT;VABRSD;VABIND;VABCAD;VABPRD;VABLOD;VABNZD;W_VABNSP;
164900080404FILL01;VABNCL;VABPKB;VABCAS;FILL02;W_VABCTR
165000080404
