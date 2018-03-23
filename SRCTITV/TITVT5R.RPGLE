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
001900040706     D tisi95ds      e ds
002000010330     D esito           s              1
002100010330     D prmlit          s             10
002200010330     D prmfir          s             10
002300010330     D wrkesito        s                   like(esito)
002400010330     D rrnum           s              6  0 INZ(*zeros)
002500040119     D parccm          s              8    INZ(*blanks)
002600040119     D parmbr          s             10    INZ(*blanks)
002700040119     D paropz          s              1    INZ(*blanks)
002800040119     D chkcall         s              1    INZ(*blanks)
002900040706     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
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
004200030822     D*------------------
004300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004400030822     D*------------------
004500030822     D posDaDft        S              3  0 INZ(*zeros)
004600030822     D posADft         S              3  0 INZ(*zeros)
004700030822     D j               s              3  0 INZ(1)
004800030822     D wGiroDft        s              1  0 INZ(*zeros)
004900010330     D*------------------
005000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005100010330     D*------------------
005200010330     D WLBDA8          DS                  INZ
005300010330     D  G08DAT                 1      8  0
005400010330     D  G08INV                 9     16  0
005500010330     D  G08ERR                17     17
005600010330     D  G08TGI                18     22  0
005700060628     D*------------------
005800060628     D* DS REPERIMENTO NUMERATORE
005900060628     D*------------------
006000060628     D trul33ds      e ds                  inz
006100060628     D*------------------
006200060628     D* DS ARCHITETTURA
006300060628     D*------------------
006400060628     D kpjba         e ds                  inz
006500031201     D*------------------
006600031201     D* Costanti
006700031201     D*------------------
006800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007000010330
007100010330
007200010330
007300000913     C                   reset                   rrnum
007400990921     C                   reset                   esito
007500990921     C                   reset                   wrkesito
007600010601     C*
007700010601     C                   exsr      opeini
007800010605     C                   exsr      rwvab
007900010601     C*
008000010601     C                   seton                                        lr
008100010601
008200010601
008300010601
008400010601     C*--------------------------------------------------------
008500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008600010601     C*--------------------------------------------------------
008700010601     C     PREELA        BEGSR
008800010601     C*
008900010601     C* SE OCCORRE SPEDIRE IN FILIALE
009000010601     C                   if        invfil <> *zeros and
009100010601     C                             flgGiro = '0'
009200010601     C*
009300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009400010601     C                   eval      flgGiro = '1'
009500010601     C*
009600010601     C                   endif
009700010601     C*
009800010601     C                   ENDSR
009900010601     C***
010000010601
010100010601
010200010601
010300010601
010400010601     C*--------------------------------------------------------
010500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010600010601     C*--------------------------------------------------------
010700010601     C     ENDELA        BEGSR
010800040119     C*
010900040119     C* Effettuo la chiamata al CLLE preposto
011000040119     C                   call(e)   'TITVVTC'
011100040119     C                   parm                    parccm
011200040119     C                   parm                    parmbr
011300040119     C                   parm      '2'           paropz
011400050201     C*
011500050201     C* Effettuo lancio TISI95 solo x chiusura
011600050201     C                   CLEAR                   TISI95DS
011700050201     C                   EVAL      I95TLA = 'C'
011800050201     C                   CALL      'TISI95R'
011900050201     C                   PARM                    TISI95DS
012000000616     C*
012100010601     C                   ENDSR
012200010601     C***
012300000613
012400010601
012500010601
012600010330     C*--------------------------------------------------------
012700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
012800010330     C*--------------------------------------------------------
012900010605     C     RWVAB         BEGSR
013000010330     C*
013100010330     C                   if        not %open(tivin00r)
013200010330     C                   open      tivin00r
013300010330     C                   endif
013400021025     C                   if        not %open(fivabwwr)
013500021025     C                   open      fivabwwr
013600010330     C                   endif
013700040119     C*
013800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013900040119     C                   exsr      prevat
014000040119     C*
014100040119     C                   if        chkcall = '0'
014200040119     C*
014300040119     C                   if        not %open(fivatwwr)
014400040119     C                   open      fivatwwr
014500040119     C                   endif
014600010330     C*
014700040119     C                   clear                   §CTROKVB          7 0
014800040119     C                   clear                   §CTROKVT          7 0
014900040119     C                   clear                   §CTRMO            7 0
015000040119     C                   clear                   §CTRNO            7 0
015100050330     C*
015200050330     C                   do        *HIVAL
015300010330     C*
015400040706     C                   read      tivin00r                               70
015500010618     C*
015600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015700010618     C                   if        vinflg = '1'
015800010618     C                   eval      flgOk = '1'
015900010618     C                   endif
016000050330     C*
016100050330     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016200050330     C                   z-add     *zeros        wLenVINDTA        4 0
016300050330     C                   z-add     *zeros        wFlgVINDTA        1 0
016400050330     C                   dow       wLenVINDTA < %len(%trim(vindta))
016500050330     C                   eval      wLenVINDTA = wLenVINDTA + 1
016600050330     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016700050330     C                   z-add     1             wFlgVINDTA
016800050330     C                   leave
016900050330     C                   endif
017000050330     C                   enddo
017100010618     C*
017200050330     C                   if        vindta > *blanks AND
017300050330     C                             wFlgVINDTA = 1
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
019500040119     C                   move(P)   invfil        VATFGS
019600021025     C                   else
019700030822     C                   move(P)   vlrpoi        VABFGS
019800040119     C                   move(P)   vlrpoi        VATFGS
019900021025     C                   endif
020000010601     C*
020100010601     C                   exsr      PREELA
020200010601     C*
020300010604     C* Ebbene...
020400010604     C*
020500040119     C  N31              add       1             §CTROKVB
020600040119     C   32              add       1             §CTRMO
020700040119     C   31              add       1             §CTRNO
020800030822     C                   if        wGiro = 2
020900040119     C  N31              exsr      wrivab
021000040119     C                   endif
021100010604     C*
021200010604     C                   if        *in31 = *off and
021300010604     C                             *in32 = *off
021400010604     C                   eval      vinflg = '1'
021500010604     C                   else
021600010604     C                   eval      vinflg = '2'
021700010604     C                   endif
021800010604     C                   endif
021900010601     C*
022000010604     C                   endif
022100010604     C*
022200010330     C                   else
022300010330     C                   eval      vinflg = '1'
022400010330     C                   endif
022500010601     C*
022600010601     C  N70              update    tivin000
022700010330     C*
022800030822     C  N70              enddo
022900040119     C*
023000040119     C                   endif                                                  (endif - chkcall)
023100010601     C*
023200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023300010601     C                   if        cntNonEl = *zeros or
023400010601     C                             flgMulti = '0'
023500010330     C* Se non ci sono record con errori ...
023600010601     C                   if        §ctrno = 0 and
023700010604     C                             §ctrmo = 0 and
023800010601     C                             flgStato <> '2'
023900010330     C* ... restituisco esito OK.
024000010330     C                   eval      wrkesito = '0'
024100010330     C                   else
024200040119     C                   if        §ctrokvb > 0
024300010330     C                   eval      wrkesito = '1'
024400010330     C                   else
024500010615     C                   if        flgOk = '0'
024600010615     C                   eval      wrkesito = '2'
024700010615     C                   else
024800010615     C                   eval      wrkesito = '6'
024900010615     C                   endif
025000010330     C                   endif
025100010330     C                   endif
025200010601     C                   else
025300010601     C                   eval      wrkesito = '9'
025400010601     C                   endif
025500010330     C*
025600010330     C                   if        %open(tivin00r)
025700010330     C                   close     tivin00r
025800010330     C                   endif
025900021025     C                   if        %open(fivabwwr)
026000021025     C                   close     fivabwwr
026100010330     C                   endif
026200040512     C                   if        %open(fivatwwr)
026300040512     C                   close     fivatwwr
026400040512     C                   endif
026500010601     C*
026600010601     C                   if        vlrpoi <> 999
026700010601     C                   eval      invfil = vlrpoi
026800010601     C                   endif
026900010330     C*
027000040119     C                   if        §ctrokvb > 0
027100010601     C                             and invfil > *zeros
027200010330     C                   exsr      invio
027300010330     C                   endif
027400010601     C*
027500010618     C                   if        flgGiro = '1'
027600010601     C                   exsr      endela
027700010618     C                   endif
027800010330     C*
027900010330     C                   ENDSR
028000010330     C***
028100040119
028200040119
028300040119
028400040119     C*----------------------------------------------------*
028500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
028600040119     C*----------------------------------------------------*
028700040119     C     WRIVAB        BEGSR
028800040119     C*
028900040119     C                   write     fivab000                                     => scarico il VAB
029000040119     C*
029100060628     C* Eseguo la scrittura del'estensione testata bolle
029200060628     C                   exsr      wrivat                                       => scarico il VAT
029300040119     C*
029400040119     C                   ENDSR
029500040119
029600040119
029700040119
029800040119     C*----------------------------------------------------*
029900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
030000040119     C*----------------------------------------------------*
030100040706     C     WRIVAT        BEGSR
030200040706     C*
030300040706     C* Attribuisco lo stesso numero numero spedizione del file testate
030400060628     C                   EVAL      VATNSP = VABNSP
030500040119     C*
030600060628     C                   EVAL      VATTRC = 'B'
030700060628     C*
030800060628     C* Scarico il buffer solo se valorizzato
030900060628     C                   if        VATNOT <> *blanks
031000050330     C                   write     FIVAT000
031100060628     C                   endif
031200040119     C*
031300040119     C                   ENDSR
031400040119
031500040119
031600040119
031700040119     C*----------------------------------------------------*
031800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
031900040119     C*----------------------------------------------------*
032000040119     C     PREVAT        BEGSR
032100040119     C*
032200040119     C* Compongo il nome del membro da dare al FIVATWWR
032300040119     C                   eval      parmbr = vlrhdl
032400040119     C                   movel     'M'           parmbr
032500060628     C                   eval      parccm = vlrKSC
032600040119     C                   eval      paropz = '1'
032700040119     C* Effettuo la chiamata al CLLE preposto
032800040119     C                   call(e)   'TITVVTC'
032900040119     C                   parm                    parccm
033000040119     C                   parm                    parmbr
033100040119     C                   parm                    paropz
033200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
033300040119     C                   if        %error
033400040119     C                   movel     '1'           chkcall
033500040119     C                   else
033600040119     C                   movel     '0'           chkcall
033700040119     C                   endif
033800040119     C*
033900040119     C                   ENDSR
034000010601
034100010601
034200010601
034300010330     C*----------------------------------------------------*
034400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
034500010330     C*----------------------------------------------------*
034600040119     C     INZVAR        BEGSR
034700010330     C*
034800030822     C* Inizializzo variabili di wrk
034900020204     C                   Z-ADD     *zeros        Num5_0            5 0
035000030822     C*
035100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
035200030822     C                   CLEAR                   FIVAB000
035300040119     C                   CLEAR                   FIVAT000
035400030822     C                   CLEAR                   SkSplitCSV
035500030822     C                   EVAL      i = 1
035600030822     C                   EVAL      posDa = *zeros
035700030822     C                   EVAL      posA  = *zeros
035800030822     C*
035900030822     C* Reimposto i valori di default
036000030822     C                   EXSR      DEFCAM
036100030822     C*
036200010330     C                   ENDSR
036300010330     C*----------------------------------------------------*
036400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
036500010330     C*----------------------------------------------------*
036600010330     C     DEFCAM        BEGSR
036700040119     C*
036800040119     C* Imposto i valori d default a programma dei campi fondamentali
036900060628     C                   EVAL      VABCCM = 1200960
037000060628     C                   EVAL      VATCCM = 1200960
037100060628     C                   EVAL      VABLNP = 120
037200060628     C                   EVAL      VATLNP = 120
037300060628     C                   EVAL      VABCTR = 300
037400040119     C                   EVAL      VABCBO = '1'
037500060703     C                   EVAL      VABVAS = 'EUR'
037600060703     C                   EVAL      VABVCA = 'EUR'
037700030822     C*
037800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
037900030715     C* e delimitatore testo.
038000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
038100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
038200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
038300050330     C*
038400050330     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
038500050330     C                   IF        %subst(vlrppt:1:1) = 'S'
038600050330     C                   SETON                                        50
038700050330     C                   ELSE
038800050330     C                   SETOFF                                       50
038900050330     C                   ENDIF
039000030822     C*
039100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
039200030822     C                   EVAL      posDaDft = 1
039300030822     C                   EVAL      posADft  = 0
039400030822     C                   EVAL      wGiroDft = 0
039500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
039600030822     C                             posDaDft > 0
039700030822     C*
039800030822     C* Gestisco il 1° giro
039900030822     C                   IF        wGiroDft = 0
040000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
040100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
040200030822     C* Incremento il contatore dei "giri"
040300030822     C                   EVAL      wGiroDft = 1
040400030822     C                   ELSE
040500030822     C                   EVAL      posDaDft = posADft
040600030822     C                   ENDIF
040700030822     C* Eseguo lo scan x trovare la fine del campo corrente
040800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
040900030822     C*
041000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
041100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
041200030822     C                   IF        posDaDft > 0 AND
041300030822     C                             posADft  > 0
041400030822     C* NCL
041500030822     C                   IF        %subst(
041600030822     C                             %subst(vlrppt:posDaDft+1:
041700030822     C                             posADft-posDaDft-1):1:3)
041800030822     C                             = 'NCL'
041900030822     C                   EVAL      PiStr=%trim(%subst(
042000030822     C                             %subst(vlrppt:posDaDft+1:
042100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042200030822     C                   EXSR      CHKNUM
042300030822     C                   IF        PiInt=*on
042400030822     C                   Z-ADD     PiVal         VABNCL
042500030822     C                   ENDIF
042600030822     C                   ENDIF
042700030822     C* CCM
042800030822     C                   IF        %subst(
042900030822     C                             %subst(vlrppt:posDaDft+1:
043000030822     C                             posADft-posDaDft-1):1:3)
043100030822     C                             = 'CCM'
043200030822     C                   EVAL      PiStr=%trim(%subst(
043300030822     C                             %subst(vlrppt:posDaDft+1:
043400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043500030822     C                   EXSR      CHKNUM
043600030822     C                   IF        PiInt=*on
043700030822     C                   Z-ADD     PiVal         VABCCM
043800040119     C                   Z-ADD     PiVal         VATCCM
043900030822     C                   ENDIF
044000030822     C                   ENDIF
044100030822     C* LNP
044200030822     C                   IF        %subst(
044300030822     C                             %subst(vlrppt:posDaDft+1:
044400030822     C                             posADft-posDaDft-1):1:3)
044500030822     C                             = 'LNP'
044600030822     C                   EVAL      PiStr=%trim(%subst(
044700030822     C                             %subst(vlrppt:posDaDft+1:
044800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044900030822     C                   EXSR      CHKNUM
045000030822     C                   IF        PiInt=*on
045100030822     C                   Z-ADD     PiVal         VABLNP
045200040119     C                   Z-ADD     PiVal         VATLNP
045300030822     C                   ENDIF
045400030822     C                   ENDIF
045500030822     C* NRS
045600030822     C                   IF        %subst(
045700030822     C                             %subst(vlrppt:posDaDft+1:
045800030822     C                             posADft-posDaDft-1):1:3)
045900030822     C                             = 'NRS'
046000030822     C                   EVAL      PiStr=%trim(%subst(
046100030822     C                             %subst(vlrppt:posDaDft+1:
046200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046300030822     C                   EXSR      CHKNUM
046400030822     C                   IF        PiInt=*on
046500030822     C                   Z-ADD     PiVal         VABNRS
046600030822     C                   ENDIF
046700030822     C                   ENDIF
046800030822     C* CTR
046900030822     C                   IF        %subst(
047000030822     C                             %subst(vlrppt:posDaDft+1:
047100030822     C                             posADft-posDaDft-1):1:3)
047200030822     C                             = 'CTR'
047300030822     C                   EVAL      PiStr=%trim(%subst(
047400030822     C                             %subst(vlrppt:posDaDft+1:
047500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047600030822     C                   EXSR      CHKNUM
047700030822     C                   IF        PiInt=*on
047800030822     C                   Z-ADD     PiVal         VABCTR
047900030822     C                   ENDIF
048000030822     C                   ENDIF
048100030822     C* PKB
048200030822     C                   IF        %subst(
048300030822     C                             %subst(vlrppt:posDaDft+1:
048400030822     C                             posADft-posDaDft-1):1:3)
048500030822     C                             = 'PKB'
048600030822     C                   EVAL      PiStr=%trim(%subst(
048700030822     C                             %subst(vlrppt:posDaDft+1:
048800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048900030822     C                   EXSR      CHKNUM
049000030822     C                   IF        PiNum=*on
049100050330     C                   Z-ADD     PiVal         VABPKB
049200030822     C                   ENDIF
049300030822     C                   ENDIF
049400030822     C* VLB
049500030822     C                   IF        %subst(
049600030822     C                             %subst(vlrppt:posDaDft+1:
049700030822     C                             posADft-posDaDft-1):1:3)
049800030822     C                             = 'VLB'
049900030822     C                   EVAL      PiStr=%trim(%subst(
050000030822     C                             %subst(vlrppt:posDaDft+1:
050100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050200030822     C                   EXSR      CHKNUM
050300030822     C                   IF        PiNum=*on
050400030822     C                   Z-ADD     PiVal         VABVLB
050500030822     C                   ENDIF
050600030822     C                   ENDIF
050700030822     C* QFT
050800030822     C                   IF        %subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):1:3)
051100030822     C                             = 'QFT'
051200030822     C                   EVAL      PiStr=%trim(%subst(
051300030822     C                             %subst(vlrppt:posDaDft+1:
051400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051500030822     C                   EXSR      CHKNUM
051600030822     C                   IF        PiNum=*on
051700030822     C                   Z-ADD     PiVal         VABQFT
051800030822     C                   ENDIF
051900030822     C                   ENDIF
052000030822     C* CBO
052100030822     C                   IF        %subst(
052200030822     C                             %subst(vlrppt:posDaDft+1:
052300030822     C                             posADft-posDaDft-1):1:3)
052400030822     C                             = 'CBO'
052500030822     C                   EVAL      VABCBO=%trim(%subst(
052600030822     C                             %subst(vlrppt:posDaDft+1:
052700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052800030822     C                   ENDIF
052900030822     C* TSP
053000030822     C                   IF        %subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):1:3)
053300030822     C                             = 'TSP'
053400030822     C                   EVAL      VABTSP=%trim(%subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053700030822     C                   ENDIF
053800030822     C* VAS
053900030822     C                   IF        %subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):1:3)
054200030822     C                             = 'VAS'
054300030822     C                   EVAL      VABVAS=%trim(%subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600030822     C                   ENDIF
054700030822     C* VCA
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'VCA'
055200030822     C                   EVAL      VABVCA=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   ENDIF
055600030822     C* GCA
055700030822     C                   IF        %subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):1:3)
056000030822     C                             = 'GCA'
056100030822     C                   EVAL      VABGCA=%trim(%subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400030822     C                   ENDIF
056500030822     C* CTM
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'CTM'
057000030822     C                   EVAL      VABCTM=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   ENDIF
057400030822     C* FFD
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'FFD'
057900030822     C                   EVAL      VABFFD=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   ENDIF
058300030822     C* VAD
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'VAD'
058800030822     C                   EVAL      VABVAD=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   ENDIF
059200030822     C* GMA
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'GMA'
059700030822     C                   EVAL      VABGMA=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   ENDIF
060100030822     C* GGA
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'GGA'
060600030822     C                   EVAL      VABGGA=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   ENDIF
061000030822     C* GVA
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'GVA'
061500030822     C                   EVAL      VABGVA=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   ENDIF
061900030822     C* TC1
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'TC1'
062400030822     C                   EVAL      VABTC1=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   ENDIF
062800030822     C* TC2
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'TC2'
063300030822     C                   EVAL      VABTC2=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* ...
063800030822     C                   ENDIF
063900030822     C                   ENDDO
064000020204     C*
064100010330     C                   ENDSR
064200010607     C*----------------------------------------------------*
064300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
064400010607     C*----------------------------------------------------*
064500010607     C     IMPVAB        BEGSR
064600010607     C*
064700010607     C                   SETOFF                                       3132
064800030515     C*
064900010607     C                   MOVEL     datcor        VABAAS
065000040119     C                   MOVEL     datcor        VATAAS
065100010607     C                   MOVE      datcor        VABMGS
065200010607     C*
065300030715     C**********
065400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
065500030715     C**********
065600030715     C                   IF        wGiro = *zeros
065700040706     C*
065800040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
065900040706     C                   EVAL      depvinDTA = vinDTA
066000040113     C*
066100040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
066200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
066300040113     C                                      %trim(intcol(3))
066400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
066500031201     C     minu:maiu     XLATE     vindta        vindta
066600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
066700030715     C                   DOW       posDa <= %len(%trim(vindta))
066800030715     C*
066900030715     C* Gestisco il 1° campo
067000030715     C                   IF        i = 1
067100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
067200030715     C                   EVAL      posDa = 1
067300030715     C* Eseguo lo scan x trovare la fine del primo campo
067400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
067500030715     C* A questo punto "estraggo" il campo corrente
067600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
067700030715     C                                                    (posA-posDa))
067800030715     C* X i campi successivi al 1°
067900030715     C                   ELSE
068000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
068100030715     C                   EVAL      posDa = posA + 1
068200030715     C* Eseguo lo scan x trovare la fine del campo corrente
068300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
068400030715     C* Gestisco l'ultimo campo
068500030715     C                   IF        posA = *zeros
068600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
068700030715     C* A questo punto "estraggo" il campo corrente
068800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
068900030715     C                                                    (posA-posDa))
069000030715     C                   LEAVE
069100030715     C                   ELSE
069200030715     C* A questo punto "estraggo" il campo corrente
069300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
069400030715     C                                                    (posA-posDa))
069500030715     C                   ENDIF
069600030715     C                   ENDIF
069700030715     C* Incremento il contatore d campo
069800030715     C                   EVAL      i = i +1
069900030715     C                   ENDDO
070000030715     C*
070100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
070200030715     C* delimitazione testo
070300030715     C                   EVAL      i = 1
070400030715     C                   DOW       i < %elem(SkSplitFLD)
070500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
070600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
070700030715     C                   EVAL      i = i + 1
070800030715     C                   ENDDO
070900030715     C                   EVAL      wGiro = 1
071000040706     C* Ripristino la 1° riga originale
071100040706     C                   EVAL      vinDTA = depvinDTA
071200030715     C                   ELSE
071300030715     C                   EVAL      wGiro = 2
071400031204     C**********
071500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
071600031204     C* d separatore campo
071700031204     C**********
071800031204     C                   EVAL      vindta = %trim(vindta)
071900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
072000031204     C                   EVAL      vindta = %subst(vindta:2)
072100031204     C                   ENDDO
072200031204     C*
072300031204     C                   Z-ADD     *zeros        lunghInput        4 0
072400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
072500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
072600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
072700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
072800031204     C                   ENDDO
072900030715     C**********
073000030715     C* Effettuo lo split del campo dati d input
073100030715     C**********
073200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
073300030715     C                   DOW       posDa <= %len(%trim(vindta))
073400030715     C*
073500030715     C* Gestisco il 1° campo
073600030715     C                   IF        i = 1
073700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
073800030715     C                   EVAL      posDa = 1
073900030715     C* Eseguo lo scan x trovare la fine del primo campo
074000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074100030715     C* A questo punto "estraggo" il campo corrente
074200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
074300030715     C                                                    (posA-posDa))
074400030715     C* X i campi successivi al 1°
074500030715     C                   ELSE
074600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
074700030715     C                   EVAL      posDa = posA + 1
074800031204     C* Verifico che nn vi sia il campo nn valorizzato
074900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
075000031204     C* Se campo nn valorizzato skippo al prossimo
075100031204     C                   EVAL      SkSplitCSV(i) = *blanks
075200031204     C                   EVAL      posA  = posA + 1
075300031204     C                   ELSE
075400030715     C* Eseguo lo scan x trovare la fine del campo corrente
075500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075600030715     C* Gestisco l'ultimo campo
075700030715     C                   IF        posA = *zeros
075800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
075900030715     C* A questo punto "estraggo" il campo corrente
076000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
076100030715     C                                                    (posA-posDa))
076200030715     C                   LEAVE
076300030715     C                   ELSE
076400030715     C* A questo punto "estraggo" il campo corrente
076500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
076600030715     C                                                    (posA-posDa))
076700030715     C                   ENDIF
076800031204     C                   ENDIF
076900030715     C                   ENDIF
077000030715     C* Incremento il contatore d campo
077100030715     C                   EVAL      i = i +1
077200030715     C                   ENDDO
077300030715     C*
077400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
077500030715     C* delimitazione testo
077600030715     C                   EVAL      i = 1
077700030715     C                   DOW       i < %elem(SkSplitCSV)
077800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
077900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
078000030715     C                   EVAL      i = i + 1
078100030715     C                   ENDDO
078200030715     C*
078300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
078400030715     C                   EVAL      i = 1
078500030715     C                   DOW       i < %elem(SkSplitFLD)
078600030715     C*
078700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
078800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
078900030715     C                             SkSplitCSV(i) <> *blanks
079000030721     C*** ATB
079100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
079200030721     C                   EVAL      VABATB = SkSplitCSV(i)
079300030721     C                   ENDIF
079400030715     C*** RSD
079500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
079600060628     C                   EVAL      VABRSD = SkSplitCSV(i)
079700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
079800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
079900030715     C* ==
080000030715     C                   ENDIF
080100030721     C*** RD2
080200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
080300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
080400030721     C                   ENDIF
080500030715     C*** IND
080600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
080700030715     C                   EVAL      VABIND = SkSplitCSV(i)
080800030715     C                   ENDIF
080900030715     C*** LOD
081000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
081100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
081200030715     C                   ENDIF
081300030715     C*** PRD
081400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
081500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
081600030715     C                   ENDIF
081700030721     C*** NZD
081800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
081900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
082000030721     C                   ENDIF
082100030715     C*** RMA
082200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
082300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
082400030715     C                   ENDIF
082500030715     C*** NOT
082600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
082700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
082800030715     C                   ENDIF
082900030715     C*** NT2
083000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
083100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
083200030715     C                   ENDIF
083300030715     C*** VCA
083400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
083500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
083600030715     C                   ENDIF
083700030715     C*** VAS
083800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
083900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
084000030715     C                   ENDIF
084100030715     C*** FFD
084200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
084300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
084400030715     C                   ENDIF
084500030721     C*** GC1
084600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
084700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
084800030721     C                   ENDIF
084900030721     C*** GC2
085000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
085100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
085200030721     C                   ENDIF
085300030721     C*** TSP
085400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
085500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
085600030721     C                   ENDIF
085700030721     C*** NAS
085800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
085900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
086000030721     C                   ENDIF
086100030721     C*** TIC
086200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
086300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
086400030721     C                   ENDIF
086500030721     C*** GCA
086600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
086700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
086800030721     C                   ENDIF
086900030721     C*** XCO
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
087100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030721     C*** CTM
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
087500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030721     C*** TCR
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
087900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030721     C*** CTS
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
088300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030721     C*** FTM
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
088700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
088800030721     C                   ENDIF
088900030721     C*** VAD
089000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
089100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
089200030721     C                   ENDIF
089300030721     C*** GMA
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
089500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030721     C*** GGA
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
089900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
090000030721     C                   ENDIF
090100030721     C*** GVA
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
090300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030721     C*** TC1
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
090700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
090800030721     C                   ENDIF
090900030721     C*** TC2
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
091100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030721     C*** SCL
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
091500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030721     C*** RMO
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
091900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030721     C*** NMO
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
092300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
092400030721     C                   ENDIF
092500040119     C*** CBO
092600040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
092700040706     C                   EVAL      VABCBO = SkSplitCSV(i)
092800040119     C                   ENDIF
092900030721     C*
093000040706     C* Reperisco quindi i campi numerici...
093100040706     C*** NSP
093200040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
093300040706     C                   EVAL      PiStr=SkSplitCSV(i)
093400040706     C                   EXSR      CHKNUM
093500040706     C                   IF        PiInt=*on
093600040706     C                   Z-ADD     PiVal         VABNSP
093700040706     C                   ELSE
093800040706     C                   SETON                                        31
093900040706     C                   EVAL      VABNSP = *zeros
094000040706     C                   EVAL      vinmsg = %trimr(vinmsg)
094100040706     C                             + ' ' + 'VABNSP'
094200040706     C                   ENDIF
094300040706     C                   ENDIF
094400060628     C*** RMN
094500040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
094600040706     C                   EVAL      PiStr=SkSplitCSV(i)
094700060628     C                   EVAL      PiStr=%subst(PiStr:1:2)+
094800060628     C                                   %subst(PiStr:4:2)+
094900060628     C                                   %subst(PiStr:7:4)
095000040706     C                   EXSR      CHKNUM
095100040706     C                   IF        PiInt=*on
095200040706     C                   Z-ADD     PiVal         VABRMN
095300040706     C                   ELSE
095400040706     C                   SETON                                        32
095500040706     C                   EVAL      VABRMN = 1
095600040706     C                   EVAL      vinmsg = %trimr(vinmsg)
095700040706     C                             + ' ' + 'VABRMN'
095800040706     C                   ENDIF
095900040706     C                   ENDIF
096000030721     C*** CAD
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
096200030721     C                   EVAL      PiStr=SkSplitCSV(i)
096300030721     C                   EXSR      CHKNUM
096400030721     C                   IF        PiInt=*on
096500030721     C                   Z-ADD     PiVal         Num5_0
096600030721     C                   MOVEL(p)  Num5_0        VABCAD
096700030721     C                   ELSE
096800030721     C                   SETON                                        32
096900030721     C                   EVAL      VABCAD = *zeros
097000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097100030721     C                             + ' ' + 'VABCAD'
097200030721     C                   ENDIF
097300030721     C                   ENDIF
097400030721     C*** NCL
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
097600030721     C                   EVAL      PiStr=SkSplitCSV(i)
097700030721     C                   EXSR      CHKNUM
097800030721     C                   IF        PiInt=*on
097900030721     C                   Z-ADD     PiVal         VABNCL
098000030721     C                   ELSE
098100030721     C                   SETON                                        32
098200030721     C                   EVAL      VABNCL = *zeros
098300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098400040706     C                             + ' ' + 'VABNCL'
098500030721     C                   ENDIF
098600030721     C                   ENDIF
098700030721     C*** PKB
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
098900060628     C                   EVAL      PiStr=SkSplitCSV(i)
099000060628     C     'g':' '       XLATE     PiStr         PiStr
099100060628     C     'G':' '       XLATE     PiStr         PiStr
099200030721     C                   EXSR      CHKNUM
099300030721     C                   IF        PiNum=*on
099400060628     C                   EVAL(H)   VABPKB = PiVal / 1000                        * da grammi a Kg
099500030721     C                   ELSE
099600030721     C                   SETON                                        32
099700030721     C                   EVAL      VABPKB = *zeros
099800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099900030721     C                             + ' ' + 'VABPKB'
100000030721     C                   ENDIF
100100030721     C                   ENDIF
100200030721     C*** CAS
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
100400030721     C                   EVAL      PiStr=SkSplitCSV(i)
100500030721     C                   EXSR      CHKNUM
100600030721     C                   IF        PiNum=*on
100700030721     C                   Z-ADD     PiVal         VABCAS
100800030721     C                   ELSE
100900030721     C                   SETON                                        32
101000030721     C                   EVAL      VABCAS = *zeros
101100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101200030721     C                             + ' ' + 'VABCAS'
101300030721     C                   ENDIF
101400030721     C                   ENDIF
101500030721     C*** CCM
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
101700030721     C                   EVAL      PiStr=SkSplitCSV(i)
101800030721     C                   EXSR      CHKNUM
101900030721     C                   IF        PiInt=*on      AND
102000030721     C                             PiVal<=9999999 AND
102100030721     C                             PiVal>*zeros
102200030721     C                   Z-ADD     PiVal         VABCCM
102300040119     C                   Z-ADD     PiVal         VATCCM
102400030721     C                   ELSE
102500030721     C                   SETON                                        32
102600030721     C                   EVAL      VABCCM = *zeros
102700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102800040119     C                             + ' ' + 'VABCCM VATCCM'
102900030721     C                   ENDIF
103000030721     C                   ENDIF
103100030721     C*** LNP
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
103300030721     C                   EVAL      PiStr=SkSplitCSV(i)
103400030721     C                   EXSR      CHKNUM
103500030721     C                   IF        PiInt=*on    AND
103600030721     C                             PiVal<=999   AND
103700030721     C                             PiVal>*zeros
103800030721     C                   Z-ADD     PiVal         VABLNP
103900040119     C                   Z-ADD     PiVal         VATLNP
104000030721     C                   ELSE
104100030721     C                   SETON                                        32
104200030721     C                   EVAL      VABLNP = *zeros
104300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104400040119     C                             + ' ' + 'VABLNP VATLNP'
104500030721     C                   ENDIF
104600030721     C                   ENDIF
104700030721     C*** NRS
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
104900030721     C                   EVAL      PiStr=SkSplitCSV(i)
105000030721     C                   EXSR      CHKNUM
105100030721     C                   IF        PiInt=*on AND
105200030721     C                             PiVal<=99
105300030721     C                   Z-ADD     PiVal         VABNRS
105400030721     C                   ELSE
105500030721     C                   SETON                                        32
105600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105700030721     C                             + ' ' + 'VABNRS'
105800030721     C                   ENDIF
105900030721     C                   ENDIF
106000030721     C*** LNA
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
106200030721     C                   EVAL      PiStr=SkSplitCSV(i)
106300030721     C                   EXSR      CHKNUM
106400030721     C                   IF        PiInt=*on    AND
106500030721     C                             PiVal<=999   AND
106600030721     C                             PiVal>*zeros
106700030721     C                   Z-ADD     PiVal         VABLNA
106800030721     C                   ELSE
106900030721     C                   SETON                                        32
107000030721     C                   EVAL      VABLNA = *zeros
107100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107200030721     C                             + ' ' + 'VABLNA'
107300030721     C                   ENDIF
107400030721     C                   ENDIF
107500030721     C*** CTR
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
107700030721     C                   EVAL      PiStr=SkSplitCSV(i)
107800030721     C                   EXSR      CHKNUM
107900030721     C                   IF        PiInt=*on  AND
108000030721     C                             PiVal<=999
108100030721     C                   Z-ADD     PiVal         VABCTR
108200030721     C                   ELSE
108300030721     C                   SETON                                        32
108400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108500030721     C                             + ' ' + 'VABCTR'
108600030721     C                   ENDIF
108700030721     C                   ENDIF
108800030721     C*** IAS
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
109000030721     C                   EVAL      PiStr=SkSplitCSV(i)
109100030721     C                   EXSR      CHKNUM
109200030721     C                   IF        PiNum=*on
109300030721     C                   Z-ADD     PiVal         VABIAS
109400030721     C                   ELSE
109500030721     C                   SETON                                        32
109600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109700030721     C                             + ' ' + 'VABIAS'
109800030721     C                   ENDIF
109900030721     C                   ENDIF
110000030721     C*** VLB
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
110200030721     C                   EVAL      PiStr=SkSplitCSV(i)
110300030721     C                   EXSR      CHKNUM
110400030721     C                   IF        PiNum=*on
110500030721     C                   Z-ADD     PiVal         VABVLB
110600030721     C                   ELSE
110700030721     C                   SETON                                        32
110800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110900030721     C                             + ' ' + 'VABVLB'
111000030721     C                   ENDIF
111100030721     C                   ENDIF
111200030721     C*** QFT
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiNum=*on
111700030721     C                   Z-ADD     PiVal         VABQFT
111800030721     C                   ELSE
111900030721     C                   SETON                                        32
112000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112100030721     C                             + ' ' + 'VABQFT'
112200030721     C                   ENDIF
112300030721     C                   ENDIF
112400030721     C*** NCD
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiInt=*on      AND
112900030721     C                             PiVal<=9999999
113000030721     C                   Z-ADD     PiVal         VABNCD
113100030721     C                   ELSE
113200030721     C                   SETON                                        32
113300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113400030721     C                             + ' ' + 'VABNCD'
113500030721     C                   ENDIF
113600030721     C                   ENDIF
113700030721     C*** NCA
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100030721     C                   IF        PiInt=*on      AND
114200030721     C                             PiVal<=9999999
114300030721     C                   Z-ADD     PiVal         VABNCA
114400030721     C                   ELSE
114500030721     C                   SETON                                        32
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700030721     C                             + ' ' + 'VABNCA'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** ZNC
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiInt=*on AND
115500030721     C                             PiVal<=99
115600030721     C                   Z-ADD     PiVal         VABZNC
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116000030721     C                             + ' ' + 'VABZNC'
116100030721     C                   ENDIF
116200030721     C                   ENDIF
116300030721     C*** DCR
116400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700030721     C                   IF        PiInt=*on       AND
116800030721     C                             PiVal<=99999999
116900030721     C                   Z-ADD     PiVal         VABDCR
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300030721     C                             + ' ' + 'VABDCR'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** HCR
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on   AND
118100030721     C                             PiVal<=9999
118200030721     C                   Z-ADD     PiVal         VABHCR
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600030721     C                             + ' ' + 'VABHCR'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** VMD
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiNum=*on
119400030721     C                   Z-ADD     PiVal         VABVMD
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800030721     C                             + ' ' + 'VABVMD'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** ANT
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiInt=*on        AND
120600030721     C                             PiVal<=999999999
120700030721     C                   Z-ADD     PiVal         VABANT
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100030721     C                             + ' ' + 'VABANT'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** CMO
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on
121900030721     C                   Z-ADD     PiVal         Num5_0
122000030721     C                   MOVEL(p)  Num5_0        VABCMO
122100030721     C                   ELSE
122200030721     C                   SETON                                        32
122300030721     C                   EVAL      VABCMO = *zeros
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500030721     C                             + ' ' + 'VABCMO'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800040706     C*
122900040706     C* Gestisco i campi "personalizzati"
123000060628     C*** TELEFONO
123100060628     C                   IF        %trim(SkSplitFLD(i)) = 'TELEFONO'
123200060628     C                   EVAL      VATNOT = SkSplitCSV(i)
123300060704     C                   EVAL      VABNOT = SkSplitCSV(i)
123400030715     C                   ENDIF
123500060628     C*
123600060628     C                   ENDIF
123700030715     C                   EVAL      i = i + 1
123800030715     C                   ENDDO
123900040706     C*
124000040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
124100040706     C* Reperisco la provincia dal CAP e dalla località
124200040706     C                   IF        VABCAD <> *blanks
124300040706     C                   CLEAR                   TISI95DS
124400040706     C                   EVAL      I95TCN = '3'
124500040706     C                   Z-ADD     datcor        I95DAT
124600040706     C                   EVAL      I95CAP = VABCAD
124700040706     C                   EVAL      I95LOC = VABLOD
124800040706     C                   CALL      'TISI95R'
124900040706     C                   PARM                    TISI95DS
125000040706     C                   EVAL      VABPRD = O95PRV
125100040706     C                   ENDIF
125200060628     C*
125300060628     C* NSP => Stacco un numeratore da AZNUM (anche se Disk A mi serve x correlare i record
125400060628     C*        del file di estensione FIVAT)
125500060628     C                   clear                   TRUL33DS
125600060628     C                   eval      I33OPE = *zeros
125700060628     C                   eval      I33CNU = 302
125800060628     C                   eval      I33NUM = 1
125900060628     C                   movel     TRUL33DS      KPJBU
126000060628     C                   call      'TRUL33R'
126100060628     C                   parm                    KPJBA
126200060628     C                   movel     KPJBU         TRUL33DS
126300060628     C                   if        O33ERR = *zeros
126400060628     C                   z-add     O33NRF        VABNSP
126500060628     C                   z-add     O33NRF        VATNSP
126600060628     C                   else
126700060628     C                   seton                                        31
126800060628     C                   eval      vinmsg = %trimr(vinmsg)
126900060628     C                             + ' ' + 'VABNSP VATNSP'
127000060628     C                   endif
127100040706     C*
127200040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
127300040706     C* d valorizzarli sempre entrambi
127400040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
127500040706     C                   Z-ADD     VABNSP        VABRMN
127600040706     C                   ENDIF
127700040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
127800040706     C                   MOVEL     VABRMN        VABRMA
127900040706     C                   ENDIF
128000040113     C*
128100040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
128200040113     C                   IF        VABCAS > *zeros
128300040119     C                   IF        VABCBO = '1'
128400040113     C                   EVAL      VABCBO = '4'
128500040119     C                   ENDIF
128600040119     C                   IF        VABCBO = '2'
128700040119     C                   EVAL      VABCBO = '6'
128800040119     C                   ENDIF
128900040113     C                   ENDIF
129000020204     C*
129100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
129200020204     C                   EXSR      CHKIMPDIV
129300030715     C                   ENDIF
129400020204     C*
129500010607     C                   ENDSR
129600010607     C*----------------------------------------------------*
129700010601
129800020204
129900020204     C*----------------------------------------------------*
130000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
130100020204     C*----------------------------------------------------*
130200020204     C     CHKIMPDIV     BEGSR
130300020204     C*
130400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
130500020204     C                   Z-ADD     *zeros        wrkDec            9 9
130600020204     C*
130700020204     C* Come prima cosa effettuo considerazioni sulla divisa
130800020204     C                   IF        vabIAS > *zeros
130900020204     C                   IF        vabVAS <> 'EUR'
131000020204     C                   EVAL      vabVAS =  'ITL'
131100020204     C                   ENDIF
131200020204     C                   ENDIF
131300020204     C*
131400020204     C                   IF        vabCAS > *zeros
131500020204     C                   IF        vabVCA <> 'EUR'
131600020204     C                   EVAL      vabVCA =  'ITL'
131700020204     C                   ENDIF
131800020204     C                   ENDIF
131900020204     C*
132000020204     C                   IF        vabVMD > *zeros
132100020204     C                   IF        vabVAD <> 'EUR'
132200020204     C                   EVAL      vabVAD =  'ITL'
132300020204     C                   ENDIF
132400020204     C                   ENDIF
132500020204     C*
132600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
132700020204     C                   Z-ADD     vabIAS        wrkDec
132800020204     C                   IF        wrkDec > *zeros
132900020204     C                   IF        vabVAS = 'ITL'
133000020204     C                   EVAL      vabIAS = *zeros
133100020204     C                   ENDIF
133200020204     C                   ENDIF
133300020204     C*
133400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
133500020204     C                   Z-ADD     vabCAS        wrkDec
133600020204     C                   IF        wrkDec > *zeros
133700020204     C                   IF        vabVCA = 'ITL'
133800020204     C                   EVAL      vabCAS = *zeros
133900020204     C                   ENDIF
134000020204     C                   ENDIF
134100020204     C*
134200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
134300020204     C                   Z-ADD     vabVMD        wrkDec
134400020204     C                   IF        wrkDec > *zeros
134500020204     C                   IF        vabVAD = 'ITL'
134600020204     C                   EVAL      vabVMD = *zeros
134700020204     C                   ENDIF
134800020204     C                   ENDIF
134900020204     C*
135000020204     C                   ENDSR
135100020204     C***
135200020204
135300010330
135400010330
135500010330     C*----------------------------------------------------*
135600010330     C*  CONTROLLO NUMERICITA' CAMPI
135700010330     C*----------------------------------------------------*
135800010330     C     CHKNUM        BEGSR
135900010330     C*
136000010606     C                   IF        PiDecChr = *blanks
136100030715     C                   EVAL      PiDecChr = CharNUM
136200010606     C                   ENDIF
136300010606     C*
136400010606     C                   CALL(e)   'ISNUMERIC'
136500010330     C                   PARM                    PiStr            30
136600010606     C                   PARM                    PiDecChr          1
136700010330     C                   PARM      *ZEROS        PiVal            30 9
136800010330     C                   PARM      '0'           PiInt             1
136900010330     C                   PARM      '0'           PiNum             1
137000010330     C                   IF        %error
137100010606     C                   EVAL      PiNum=*off
137200010330     C                   ENDIF
137300010330     C*
137400010330     C                   ENDSR
137500010330     C***
137600010330
137700010601
137800010601
137900010601
138000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
138100010601     C     repfil        BEGSR
138200010601     C*
138300010601     C                   if        invfil = *zeros and
138400010601     C                             depfil > *zeros and
138500010629     C                             (vinflg = *blanks or
138600010629     C                              vinflg = *zeros)
138700010601     C
138800010601     C                   eval      invfil = depfil
138900010601     C                   endif
139000010601     C*
139100010601     C                   if        depfil <> invfil and
139200010601     C                             invfil > *zeros
139300010601     C                   eval      flgMulti = '1'
139400010601     C                   if        vinflg = *blanks
139500010601     C                   add       1             cntNonEl
139600010601     C                   endif
139700010601     C                   endif
139800010601     C*
139900010601     C                   if        vinflg = '2'
140000010601     C                   eval      flgStato = '2'
140100010601     C                   endif
140200010601     C*
140300010601     C                   ENDSR
140400010601     C***
140500010601
140600010601
140700010601
140800010330
140900040119      /TITLE Invio dei dati al punto operativo.
141000040119     C     invio         BEGSR
141100040119     C*
141200040119     C* 1° invio FIVAT
141300040119     C                   reset                   dscmz
141400040119     C                   move      vlrpoi        cmzdst
141500040119     C                   eval      cmzfld = 'FIVATWWR'
141600040119     C                   eval      cmzmbd = vlrhdl
141700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
141800040119     C***                if        prmfir = *blanks
141900040119     C                   eval      cmzfla = 'FIVAT00F'
142000040119     C                   eval      cmzmba = 'FIVAT00F'
142100040119     C***                else
142200040119     C***                eval      cmzfla = prmfir
142300040119     C***                eval      cmzmba = prmfir
142400040119     C***                endif
142500040119     C                   eval      cmznrr = *zeros
142600040119     C                   move      §ctrokvt      cmznrr
142700040119     C                   eval      cmzlba = vlrfl1
142800040119     C                   call(e)   'TIS711C'
142900040119     C                   parm                    dscmz
143000040119     C                   parm      *blanks       esito
143100040119     C                   if        %error
143200040119     C                             or cmzerr = '1'
143300040119     C                             or esito  = '1'
143400040119     C                   eval      wrkesito = '3'
143500040119     C                   else
143600040119     C*
143700040119     C* 2° invio FIVAB
143800040119     C                   reset                   dscmz
143900040119     C                   move      vlrpoi        cmzdst
144000040119     C                   eval      cmzfld = vlrfou
144100040119     C                   eval      cmzmbd = vlrhdl
144200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
144300040119     C***                if        prmfir = *blanks
144400040119     C                   eval      cmzfla = 'FIVAB00F'
144500040119     C                   eval      cmzmba = 'FIVAB00F'
144600040119     C***                else
144700040119     C***                eval      cmzfla = prmfir
144800040119     C***                eval      cmzmba = prmfir
144900040119     C***                endif
145000040119     C                   eval      cmznrr = *zeros
145100040119     C                   move      §ctrokvb      cmznrr
145200040119     C                   eval      cmzlba = vlrfl1
145300040119     C                   call(e)   'TIS711C'
145400040119     C                   parm                    dscmz
145500040119     C                   parm      *blanks       esito
145600040119     C                   if        %error
145700040119     C                             or cmzerr = '1'
145800040119     C                             or esito  = '1'
145900040119     C                   eval      wrkesito = '3'
146000040119     C                   endif
146100040119     C                   endif
146200040119     C*
146300040119     C                   ENDSR
146400040119     C***
146500010601
146600010601
146700010601
146800010601
146900010601      /TITLE Invio dei dati al punto operativo.
147000010601     C     opeini        BEGSR
147100010601     C*
147200010601     C* Inizializzo flag e contatori operativi
147300010601     C                   movel     '0'           flgGiro           1
147400010601     C                   movel     '0'           flgMulti          1
147500010601     C                   movel     '1'           flgStato          1
147600010615     C                   movel     '0'           flgOk             1
147700010601     C                   z-add     *zeros        cntNonEl         10 0
147800010601     C                   z-add     *zeros        depfil            3 0
147900010601     C                   z-add     *zeros        invfil            3 0
148000010601     C*
148100010601     C                   ENDSR
148200010601     C***
148300010601
148400010601
148500050421
148600050421
148700070411
148800070411     C     *pssr         BEGSR
148900070411     C*
149000070411     C                   if        %open(tivin00r)
149100070411     C                   close     tivin00r
149200070411     C                   endif
149300070411     C                   if        %open(fivabwwr)
149400070411     C                   close     fivabwwr
149500070411     C                   endif
149600070411     C                   if        %open(fivatwwr)
149700070411     C                   close     fivatwwr
149800070411     C                   endif
149900070411     C*
150000070411     C* Effettuo la chiamata al CLLE preposto
150100070411     C                   call(e)   'TITVVTC'
150200070411     C                   parm                    parccm
150300070411     C                   parm                    parmbr
150400070411     C                   parm      '2'           paropz
150500070411     C*
150600070411     C                   eval      wrkesito = '2'
150700070411     C*
150800070411     C                   seton                                        LR
150900070411     C*
151000070411     C                   ENDSR     '*CANCL'
151100070411     C***
151200070411
151300050421
151400010601
151500010330
151600010330
151700000613     C     *inzsr        BEGSR
151800990910     C*
151900990910     C     *entry        plist
152000990920     C                   parm                    tivlrds
152100990921     C                   parm      wrkesito      esito
152200000724     C                   parm                    prmlit
152300000710     C                   parm                    prmfir
152400010330     C*
152500010330     C* CALCOLA LA DATA CORRENTE
152600010330     C                   time                    wn14             14 0
152700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
152800010330     C                   z-add     wn8           g08dat
152900010330     C                   z-add     *zeros        g08inv
153000010330     C                   movel     '0'           g08err
153100010330     C                   call      'XSRDA8'
153200010330     C                   parm                    wlbda8
153300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
153400000613     C*
153500000613     C                   ENDSR
153600000613     C***
153700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
153800060628FILLER1;VABRMN;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;VABRD2;VABCAS;
153900060704TELEFONO;VABNCL;VABRMA;VABPKB;VABNT2
154000040113
