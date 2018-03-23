000100070301      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300070208     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500070301     FEDIVABwr  O    E             DISK    usropn
000600070301     FEDIVATwr  O    E             DISK    usropn
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
003000070306     D wNomeFile       s             30    INZ(*blanks)
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
004300030822     D*------------------
004400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004500030822     D*------------------
004600030822     D posDaDft        S              3  0 INZ(*zeros)
004700030822     D posADft         S              3  0 INZ(*zeros)
004800030822     D j               s              3  0 INZ(1)
004900030822     D wGiroDft        s              1  0 INZ(*zeros)
005000010330     D*------------------
005100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005200010330     D*------------------
005300010330     D WLBDA8          DS                  INZ
005400010330     D  G08DAT                 1      8  0
005500010330     D  G08INV                 9     16  0
005600010330     D  G08ERR                17     17
005700010330     D  G08TGI                18     22  0
005800060327     D*------------------
005900060327     D* DS REPERIMENTO NUMERATORE
006000060327     D*------------------
006100060327     D trul33ds      e ds                  inz
006200060327     D*------------------
006300060327     D* DS ARCHITETTURA
006400060327     D*------------------
006500060327     D kpjba         e ds                  inz
006600031201     D*------------------
006700031201     D* Costanti
006800031201     D*------------------
006900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007100010330
007200010330
007300010330
007400000913     C                   reset                   rrnum
007500990921     C                   reset                   esito
007600990921     C                   reset                   wrkesito
007700010601     C*
007800010601     C                   exsr      opeini
007900010605     C                   exsr      rwvab
008000010601     C*
008100010601     C                   seton                                        lr
008200010601
008300010601
008400010601
008500010601     C*--------------------------------------------------------
008600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008700010601     C*--------------------------------------------------------
008800010601     C     PREELA        BEGSR
008900010601     C*
009000010601     C* SE OCCORRE SPEDIRE IN FILIALE
009100010601     C                   if        invfil <> *zeros and
009200010601     C                             flgGiro = '0'
009300010601     C*
009400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009500010601     C                   eval      flgGiro = '1'
009600010601     C*
009700010601     C                   endif
009800010601     C*
009900010601     C                   ENDSR
010000010601     C***
010100010601
010200010601
010300010601
010400010601
010500010601     C*--------------------------------------------------------
010600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010700010601     C*--------------------------------------------------------
010800010601     C     ENDELA        BEGSR
010900040119     C*
011000040119     C* Effettuo la chiamata al CLLE preposto
011100070208     C                   if        *in60 = *on
011200070301     C                   call(e)   'TITVEVTC'
011300040119     C                   parm                    parccm
011400040119     C                   parm                    parmbr
011500040119     C                   parm      '2'           paropz
011600070208     C                   endif
011700050201     C*
011800050201     C* Effettuo lancio TISI95 solo x chiusura
011900050201     C                   CLEAR                   TISI95DS
012000050201     C                   EVAL      I95TLA = 'C'
012100050201     C                   CALL      'TISI95R'
012200050201     C                   PARM                    TISI95DS
012300060327     C*
012400060327     C* Effettuo lancio TRUL33 solo x chiusura
012500060327     C                   clear                   TRUL33DS
012600060327     C                   eval      I33TLA = 'C'
012700060327     C                   movel     TRUL33DS      KPJBU
012800060327     C                   call      'TRUL33R'
012900060327     C                   parm                    KPJBA
013000000616     C*
013100010601     C                   ENDSR
013200010601     C***
013300000613
013400010601
013500010601
013600010330     C*--------------------------------------------------------
013700070301     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
013800010330     C*--------------------------------------------------------
013900010605     C     RWVAB         BEGSR
014000010330     C*
014100010330     C                   if        not %open(tivin00r)
014200010330     C                   open      tivin00r
014300010330     C                   endif
014400070301     C                   if        not %open(edivabwr)
014500070301     C                   open      edivabwr
014600010330     C                   endif
014700040119     C*
014800070301     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
014900040119     C                   exsr      prevat
015000040119     C*
015100040119     C                   if        chkcall = '0'
015200040119     C*
015300070208     C                   if        *in60 = *on
015400070301     C                   if        not %open(edivatwr)
015500070301     C                   open      edivatwr
015600070208     C                   endif
015700040119     C                   endif
015800010330     C*
015900040119     C                   clear                   §CTROKVB          7 0
016000040119     C                   clear                   §CTROKVT          7 0
016100040119     C                   clear                   §CTRMO            7 0
016200040119     C                   clear                   §CTRNO            7 0
016300050330     C*
016400050330     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016500050330     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016600070208     C                   exsr      inzvar
016700070208     C                   exsr      impvab
016800050330     C*
016900050330     C                   do        *HIVAL
017000010330     C*
017100040706     C                   read      tivin00r                               70
017200010618     C*
017300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017400010618     C                   if        vinflg = '1'
017500010618     C                   eval      flgOk = '1'
017600010618     C                   endif
017700050330     C*
017800050330     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017900050330     C                   z-add     *zeros        wLenVINDTA        4 0
018000050330     C                   z-add     *zeros        wFlgVINDTA        1 0
018100050330     C                   dow       wLenVINDTA < %len(%trim(vindta))
018200050330     C                   eval      wLenVINDTA = wLenVINDTA + 1
018300050330     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018400050330     C                   z-add     1             wFlgVINDTA
018500050330     C                   leave
018600050330     C                   endif
018700050330     C                   enddo
018800010618     C*
018900050330     C                   if        vindta > *blanks AND
019000050330     C                             wFlgVINDTA = 1
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
020300010330     C                   exsr      inzvar
020400010605     C                   exsr      impvab
020500010601     C*
020600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020700010605     C                   eval      depfil = VABLNP
020800010601     C                   exsr      repfil
020900010601     C                   if        depfil = invfil
021000021025     C                   if        vlrpoi = 999
021100030822     C                   move(P)   invfil        VABFGS
021200070208     C   60              move(P)   invfil        VATFGS
021300021025     C                   else
021400030822     C                   move(P)   vlrpoi        VABFGS
021500070208     C   60              move(P)   vlrpoi        VATFGS
021600021025     C                   endif
021700010601     C*
021800010601     C                   exsr      PREELA
021900070301     C*
022000070301     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
022100070306     C                   EVAL      VABCMR = wNomeFile
022200070301     C                   EVAL      VABDCM = DATCOR
022300070301     C                   EVAL      VABDTS = DATCOR
022400070301     C                   EVAL      VABHMS = ORACOR
022500070301     C                   EVAL      VABCNT = 1
022600070301     C*
022700070301     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
022800070306     C                   EVAL      VATCMR = wNomeFile
022900070301     C                   EVAL      VATCNT = 1
023000010601     C*
023100010604     C* Ebbene...
023200010604     C*
023300040119     C  N31              add       1             §CTROKVB
023400040119     C   32              add       1             §CTRMO
023500040119     C   31              add       1             §CTRNO
023600030822     C                   if        wGiro = 2
023700040119     C  N31              exsr      wrivab
023800040119     C                   endif
023900010604     C*
024000010604     C                   if        *in31 = *off and
024100010604     C                             *in32 = *off
024200010604     C                   eval      vinflg = '1'
024300010604     C                   else
024400010604     C                   eval      vinflg = '2'
024500010604     C                   endif
024600010604     C                   endif
024700010601     C*
024800010604     C                   endif
024900010604     C*
025000010330     C                   else
025100010330     C                   eval      vinflg = '1'
025200010330     C                   endif
025300010601     C*
025400010601     C  N70              update    tivin000
025500010330     C*
025600030822     C  N70              enddo
025700040119     C*
025800040119     C                   endif                                                  (endif - chkcall)
025900010601     C*
026000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026100010601     C                   if        cntNonEl = *zeros or
026200010601     C                             flgMulti = '0'
026300010330     C* Se non ci sono record con errori ...
026400010601     C                   if        §ctrno = 0 and
026500010604     C                             §ctrmo = 0 and
026600010601     C                             flgStato <> '2'
026700010330     C* ... restituisco esito OK.
026800010330     C                   eval      wrkesito = '0'
026900010330     C                   else
027000040119     C                   if        §ctrokvb > 0
027100010330     C                   eval      wrkesito = '1'
027200010330     C                   else
027300010615     C                   if        flgOk = '0'
027400010615     C                   eval      wrkesito = '2'
027500010615     C                   else
027600010615     C                   eval      wrkesito = '6'
027700010615     C                   endif
027800010330     C                   endif
027900010330     C                   endif
028000010601     C                   else
028100010601     C                   eval      wrkesito = '9'
028200010601     C                   endif
028300010330     C*
028400010330     C                   if        %open(tivin00r)
028500010330     C                   close     tivin00r
028600010330     C                   endif
028700070301     C                   if        %open(edivabwr)
028800070301     C                   close     edivabwr
028900010330     C                   endif
029000070208     C                   if        *in60 = *on
029100070301     C                   if        %open(edivatwr)
029200070301     C                   close     edivatwr
029300070208     C                   endif
029400040512     C                   endif
029500010601     C*
029600010601     C                   if        vlrpoi <> 999
029700010601     C                   eval      invfil = vlrpoi
029800010601     C                   endif
029900010330     C*
030000040119     C                   if        §ctrokvb > 0
030100010601     C                             and invfil > *zeros
030200010330     C                   exsr      invio
030300010330     C                   endif
030400010601     C*
030500010618     C                   if        flgGiro = '1'
030600010601     C                   exsr      endela
030700010618     C                   endif
030800010330     C*
030900010330     C                   ENDSR
031000010330     C***
031100040119
031200040119
031300040119
031400040119     C*----------------------------------------------------*
031500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
031600040119     C*----------------------------------------------------*
031700040119     C     WRIVAB        BEGSR
031800040119     C*
031900070301     C                   write     edivab00                                     => scarico il VAB
032000040119     C*
032100040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
032200070216     C   60              exsr      wrivat
032300040119     C*
032400040119     C                   ENDSR
032500040119
032600040119
032700040119
032800060327     C*----------------------------------------------------*
032900060327     C*  SCARICAMENTO BUFFER RECORDS VAT
033000060327     C*----------------------------------------------------*
033100060327     C     WRIVAT        BEGSR
033200060327     C*
033300060327     C* Attribuisco la stessa chiave della testata bolle
033400060327     C                   EVAL      VATAAS = VABAAS
033500070216     C                   EVAL      VATLNP = VABLNP
033600070216     C                   EVAL      VATNRS = VABNRS
033700070216     C                   EVAL      VATNSP = VABNSP
033800070216     C                   EVAL      VATTRC = 'E'
033900070220     C*
034000070220     C* Il barcode d ogni collo (tutte e sempre spedizioni mono-collo) coincide con il rif.mitt.alfa
034100070220     C                   EVAL      VATNOT = %trim(VABRMA)
034200060327     C*
034300060327     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
034400060327     C                   IF        VATNOT <> *blanks
034500070301     C                   WRITE     EDIVAT00
034600060327     C                   ADD       1             §CTROKVT
034700060327     C                   ENDIF
034800060327     C*
034900060327     C                   ENDSR
035000040119
035100040119
035200040119
035300040119     C*----------------------------------------------------*
035400070301     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
035500040119     C*----------------------------------------------------*
035600040119     C     PREVAT        BEGSR
035700070208     C*
035800070208     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
035900070208     C                   SETOFF                                       50
036000070208     C                   IF        %subst(vlrppt:1:1) = 'C'
036100070208     C                   SETON                                        60
036200070208     C                   ELSE
036300070208     C                   SETOFF                                       60
036400070208     C                   ENDIF
036500070208     C*
036600070208     C* Se richiesta elaborazione anxhe FNVAT => proseguo
036700070208     C                   if        *in60 = *on
036800040119     C*
036900070301     C* Compongo il nome del membro da dare al EDIVATWR
037000040119     C                   eval      parmbr = vlrhdl
037100040119     C                   movel     'M'           parmbr
037200050330     C                   eval      parccm = %subst(vlrKSC:2:7)
037300040119     C                   eval      paropz = '1'
037400040119     C* Effettuo la chiamata al CLLE preposto
037500070301     C                   call(e)   'TITVEVTC'
037600040119     C                   parm                    parccm
037700040119     C                   parm                    parmbr
037800040119     C                   parm                    paropz
037900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
038000040119     C                   if        %error
038100040119     C                   movel     '1'           chkcall
038200040119     C                   else
038300040119     C                   movel     '0'           chkcall
038400040119     C                   endif
038500070208     C*
038600070208     C                   else
038700070208     C                   movel     '0'           chkcall
038800070208     C                   endif
038900040119     C*
039000040119     C                   ENDSR
039100010601
039200010601
039300010601
039400010330     C*----------------------------------------------------*
039500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
039600010330     C*----------------------------------------------------*
039700040119     C     INZVAR        BEGSR
039800010330     C*
039900030822     C* Inizializzo variabili di wrk
040000020204     C                   Z-ADD     *zeros        Num5_0            5 0
040100030822     C*
040200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
040300070301     C                   CLEAR                   EDIVAB00
040400070301     C   60              CLEAR                   EDIVAT00
040500030822     C                   CLEAR                   SkSplitCSV
040600030822     C                   EVAL      i = 1
040700030822     C                   EVAL      posDa = *zeros
040800030822     C                   EVAL      posA  = *zeros
040900030822     C*
041000030822     C* Reimposto i valori di default
041100030822     C                   EXSR      DEFCAM
041200030822     C*
041300010330     C                   ENDSR
041400010330     C*----------------------------------------------------*
041500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
041600010330     C*----------------------------------------------------*
041700010330     C     DEFCAM        BEGSR
041800040119     C*
041900040119     C* Imposto i valori d default a programma dei campi fondamentali
042000110516     C                   EVAL      VABCCM = 2494540
042100110516     C   60              EVAL      VATCCM = 2494540
042200070216     C                   EVAL      VABLNP = 249
042300070216     C   60              EVAL      VATLNP = 249
042400070216     C                   EVAL      VABCTR = 000
042500070220     C                   EVAL      VABCTM = '7Q'
042600040119     C                   EVAL      VABCBO = '1'
042700070216     C                   EVAL      VABNCL = 1
042800030822     C*
042900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
043000030715     C* e delimitatore testo.
043100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
043200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
043300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
043400030822     C*
043500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
043600030822     C                   EVAL      posDaDft = 1
043700030822     C                   EVAL      posADft  = 0
043800030822     C                   EVAL      wGiroDft = 0
043900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
044000030822     C                             posDaDft > 0
044100030822     C*
044200030822     C* Gestisco il 1° giro
044300030822     C                   IF        wGiroDft = 0
044400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
044500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
044600030822     C* Incremento il contatore dei "giri"
044700030822     C                   EVAL      wGiroDft = 1
044800030822     C                   ELSE
044900030822     C                   EVAL      posDaDft = posADft
045000030822     C                   ENDIF
045100030822     C* Eseguo lo scan x trovare la fine del campo corrente
045200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
045300030822     C*
045400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
045500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
045600030822     C                   IF        posDaDft > 0 AND
045700030822     C                             posADft  > 0
045800030822     C* NCL
045900030822     C                   IF        %subst(
046000030822     C                             %subst(vlrppt:posDaDft+1:
046100030822     C                             posADft-posDaDft-1):1:3)
046200030822     C                             = 'NCL'
046300030822     C                   EVAL      PiStr=%trim(%subst(
046400030822     C                             %subst(vlrppt:posDaDft+1:
046500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046600030822     C                   EXSR      CHKNUM
046700030822     C                   IF        PiInt=*on
046800030822     C                   Z-ADD     PiVal         VABNCL
046900030822     C                   ENDIF
047000030822     C                   ENDIF
047100030822     C* CCM
047200030822     C                   IF        %subst(
047300030822     C                             %subst(vlrppt:posDaDft+1:
047400030822     C                             posADft-posDaDft-1):1:3)
047500030822     C                             = 'CCM'
047600030822     C                   EVAL      PiStr=%trim(%subst(
047700030822     C                             %subst(vlrppt:posDaDft+1:
047800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047900030822     C                   EXSR      CHKNUM
048000030822     C                   IF        PiInt=*on
048100030822     C                   Z-ADD     PiVal         VABCCM
048200070208     C   60              Z-ADD     PiVal         VATCCM
048300030822     C                   ENDIF
048400030822     C                   ENDIF
048500030822     C* LNP
048600030822     C                   IF        %subst(
048700030822     C                             %subst(vlrppt:posDaDft+1:
048800030822     C                             posADft-posDaDft-1):1:3)
048900030822     C                             = 'LNP'
049000030822     C                   EVAL      PiStr=%trim(%subst(
049100030822     C                             %subst(vlrppt:posDaDft+1:
049200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049300030822     C                   EXSR      CHKNUM
049400030822     C                   IF        PiInt=*on
049500030822     C                   Z-ADD     PiVal         VABLNP
049600070208     C   60              Z-ADD     PiVal         VATLNP
049700030822     C                   ENDIF
049800030822     C                   ENDIF
049900030822     C* NRS
050000030822     C                   IF        %subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):1:3)
050300030822     C                             = 'NRS'
050400030822     C                   EVAL      PiStr=%trim(%subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050700030822     C                   EXSR      CHKNUM
050800030822     C                   IF        PiInt=*on
050900030822     C                   Z-ADD     PiVal         VABNRS
051000030822     C                   ENDIF
051100030822     C                   ENDIF
051200030822     C* CTR
051300030822     C                   IF        %subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):1:3)
051600030822     C                             = 'CTR'
051700030822     C                   EVAL      PiStr=%trim(%subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052000030822     C                   EXSR      CHKNUM
052100030822     C                   IF        PiInt=*on
052200030822     C                   Z-ADD     PiVal         VABCTR
052300030822     C                   ENDIF
052400030822     C                   ENDIF
052500030822     C* PKB
052600030822     C                   IF        %subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):1:3)
052900030822     C                             = 'PKB'
053000030822     C                   EVAL      PiStr=%trim(%subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300030822     C                   EXSR      CHKNUM
053400030822     C                   IF        PiNum=*on
053500050330     C                   Z-ADD     PiVal         VABPKB
053600030822     C                   ENDIF
053700030822     C                   ENDIF
053800030822     C* VLB
053900030822     C                   IF        %subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):1:3)
054200030822     C                             = 'VLB'
054300030822     C                   EVAL      PiStr=%trim(%subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600030822     C                   EXSR      CHKNUM
054700030822     C                   IF        PiNum=*on
054800030822     C                   Z-ADD     PiVal         VABVLB
054900030822     C                   ENDIF
055000030822     C                   ENDIF
055100030822     C* QFT
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'QFT'
055600030822     C                   EVAL      PiStr=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   EXSR      CHKNUM
056000030822     C                   IF        PiNum=*on
056100030822     C                   Z-ADD     PiVal         VABQFT
056200030822     C                   ENDIF
056300030822     C                   ENDIF
056400030822     C* CBO
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'CBO'
056900030822     C                   EVAL      VABCBO=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   ENDIF
057300030822     C* TSP
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'TSP'
057800030822     C                   EVAL      VABTSP=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   ENDIF
058200030822     C* VAS
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'VAS'
058700030822     C                   EVAL      VABVAS=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   ENDIF
059100030822     C* VCA
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'VCA'
059600030822     C                   EVAL      VABVCA=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   ENDIF
060000030822     C* GCA
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'GCA'
060500030822     C                   EVAL      VABGCA=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   ENDIF
060900030822     C* CTM
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'CTM'
061400030822     C                   EVAL      VABCTM=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   ENDIF
061800030822     C* FFD
061900030822     C                   IF        %subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):1:3)
062200030822     C                             = 'FFD'
062300030822     C                   EVAL      VABFFD=%trim(%subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600030822     C                   ENDIF
062700030822     C* VAD
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'VAD'
063200030822     C                   EVAL      VABVAD=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   ENDIF
063600030822     C* GMA
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'GMA'
064100030822     C                   EVAL      VABGMA=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   ENDIF
064500030822     C* GGA
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'GGA'
065000030822     C                   EVAL      VABGGA=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   ENDIF
065400030822     C* GVA
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'GVA'
065900030822     C                   EVAL      VABGVA=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   ENDIF
066300030822     C* TC1
066400030822     C                   IF        %subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):1:3)
066700030822     C                             = 'TC1'
066800030822     C                   EVAL      VABTC1=%trim(%subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100030822     C                   ENDIF
067200030822     C* TC2
067300030822     C                   IF        %subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):1:3)
067600030822     C                             = 'TC2'
067700030822     C                   EVAL      VABTC2=%trim(%subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068000030822     C                   ENDIF
068100030822     C* ...
068200030822     C                   ENDIF
068300030822     C                   ENDDO
068400020204     C*
068500010330     C                   ENDSR
068600010607     C*----------------------------------------------------*
068700070301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
068800010607     C*----------------------------------------------------*
068900010607     C     IMPVAB        BEGSR
069000010607     C*
069100010607     C                   SETOFF                                       3132
069200030515     C*
069300010607     C                   MOVEL     datcor        VABAAS
069400070208     C   60              MOVEL     datcor        VATAAS
069500010607     C                   MOVE      datcor        VABMGS
069600010607     C*
069700030715     C**********
069800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
069900030715     C**********
070000030715     C                   IF        wGiro = *zeros
070100040706     C*
070200040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
070300040706     C                   EVAL      depvinDTA = vinDTA
070400040113     C*
070500040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
070600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
070700040113     C                                      %trim(intcol(3))
070800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
070900031201     C     minu:maiu     XLATE     vindta        vindta
071000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
071100030715     C                   DOW       posDa <= %len(%trim(vindta))
071200030715     C*
071300030715     C* Gestisco il 1° campo
071400030715     C                   IF        i = 1
071500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
071600030715     C                   EVAL      posDa = 1
071700030715     C* Eseguo lo scan x trovare la fine del primo campo
071800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071900030715     C* A questo punto "estraggo" il campo corrente
072000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
072100030715     C                                                    (posA-posDa))
072200030715     C* X i campi successivi al 1°
072300030715     C                   ELSE
072400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
072500030715     C                   EVAL      posDa = posA + 1
072600030715     C* Eseguo lo scan x trovare la fine del campo corrente
072700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072800030715     C* Gestisco l'ultimo campo
072900030715     C                   IF        posA = *zeros
073000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
073100030715     C* A questo punto "estraggo" il campo corrente
073200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073300030715     C                                                    (posA-posDa))
073400030715     C                   LEAVE
073500030715     C                   ELSE
073600030715     C* A questo punto "estraggo" il campo corrente
073700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073800030715     C                                                    (posA-posDa))
073900030715     C                   ENDIF
074000030715     C                   ENDIF
074100030715     C* Incremento il contatore d campo
074200030715     C                   EVAL      i = i +1
074300030715     C                   ENDDO
074400030715     C*
074500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
074600030715     C* delimitazione testo
074700030715     C                   EVAL      i = 1
074800030715     C                   DOW       i < %elem(SkSplitFLD)
074900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
075000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
075100030715     C                   EVAL      i = i + 1
075200030715     C                   ENDDO
075300030715     C                   EVAL      wGiro = 1
075400040706     C* Ripristino la 1° riga originale
075500040706     C                   EVAL      vinDTA = depvinDTA
075600030715     C                   ELSE
075700030715     C                   EVAL      wGiro = 2
075800031204     C**********
075900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
076000031204     C* d separatore campo
076100031204     C**********
076200031204     C                   EVAL      vindta = %trim(vindta)
076300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
076400031204     C                   EVAL      vindta = %subst(vindta:2)
076500031204     C                   ENDDO
076600031204     C*
076700031204     C                   Z-ADD     *zeros        lunghInput        4 0
076800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
076900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
077000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
077100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
077200031204     C                   ENDDO
077300030715     C**********
077400030715     C* Effettuo lo split del campo dati d input
077500030715     C**********
077600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077700030715     C                   DOW       posDa <= %len(%trim(vindta))
077800030715     C*
077900030715     C* Gestisco il 1° campo
078000030715     C                   IF        i = 1
078100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078200030715     C                   EVAL      posDa = 1
078300030715     C* Eseguo lo scan x trovare la fine del primo campo
078400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078500030715     C* A questo punto "estraggo" il campo corrente
078600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078700030715     C                                                    (posA-posDa))
078800030715     C* X i campi successivi al 1°
078900030715     C                   ELSE
079000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079100030715     C                   EVAL      posDa = posA + 1
079200031204     C* Verifico che nn vi sia il campo nn valorizzato
079300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
079400031204     C* Se campo nn valorizzato skippo al prossimo
079500031204     C                   EVAL      SkSplitCSV(i) = *blanks
079600031204     C                   EVAL      posA  = posA + 1
079700031204     C                   ELSE
079800030715     C* Eseguo lo scan x trovare la fine del campo corrente
079900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080000030715     C* Gestisco l'ultimo campo
080100030715     C                   IF        posA = *zeros
080200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080300030715     C* A questo punto "estraggo" il campo corrente
080400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080500030715     C                                                    (posA-posDa))
080600030715     C                   LEAVE
080700030715     C                   ELSE
080800030715     C* A questo punto "estraggo" il campo corrente
080900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081000030715     C                                                    (posA-posDa))
081100030715     C                   ENDIF
081200031204     C                   ENDIF
081300030715     C                   ENDIF
081400030715     C* Incremento il contatore d campo
081500030715     C                   EVAL      i = i +1
081600030715     C                   ENDDO
081700030715     C*
081800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081900030715     C* delimitazione testo
082000030715     C                   EVAL      i = 1
082100030715     C                   DOW       i < %elem(SkSplitCSV)
082200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
082300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
082400030715     C                   EVAL      i = i + 1
082500030715     C                   ENDDO
082600030715     C*
082700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
082800030715     C                   EVAL      i = 1
082900030715     C                   DOW       i < %elem(SkSplitFLD)
083000030715     C*
083100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
083200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
083300030715     C                             SkSplitCSV(i) <> *blanks
083400030721     C*** ATB
083500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
083600030721     C                   EVAL      VABATB = SkSplitCSV(i)
083700030721     C                   ENDIF
083800060327     C*** RSD
083900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
084000060327     C                   EVAL      VABRSD = SkSplitCSV(i)
084100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
084200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
084300030715     C* ==
084400030715     C                   ENDIF
084500030721     C*** RD2
084600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
084700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
084800030721     C                   ENDIF
084900030715     C*** IND
085000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
085100030715     C                   EVAL      VABIND = SkSplitCSV(i)
085200030715     C                   ENDIF
085300030715     C*** LOD
085400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
085500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
085600030715     C                   ENDIF
085700030715     C*** PRD
085800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
085900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
086000030715     C                   ENDIF
086100030721     C*** NZD
086200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
086300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
086400030721     C                   ENDIF
086500030715     C*** RMA
086600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
086700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
086800030715     C                   ENDIF
086900030715     C*** NOT
087000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
087100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
087200030715     C                   ENDIF
087300030715     C*** NT2
087400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
087500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
087600030715     C                   ENDIF
087700030715     C*** VCA
087800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
087900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
088000030715     C                   ENDIF
088100030715     C*** VAS
088200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
088300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
088400030715     C                   ENDIF
088500030715     C*** FFD
088600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
088700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
088800030715     C                   ENDIF
088900030721     C*** GC1
089000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
089100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
089200030721     C                   ENDIF
089300030721     C*** GC2
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
089500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030721     C*** TSP
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
089900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
090000030721     C                   ENDIF
090100030721     C*** NAS
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
090300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030721     C*** TIC
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
090700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
090800030721     C                   ENDIF
090900030721     C*** GCA
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
091100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030721     C*** XCO
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
091500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030721     C*** CTM
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
091900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030721     C*** TCR
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
092300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
092400030721     C                   ENDIF
092500030721     C*** CTS
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
092700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030721     C*** FTM
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
093100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
093200030721     C                   ENDIF
093300030721     C*** VAD
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
093500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
093600030721     C                   ENDIF
093700030721     C*** GMA
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
093900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030721     C*** GGA
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
094300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030721     C*** GVA
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
094700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030721     C*** TC1
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
095100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
095200030721     C                   ENDIF
095300030721     C*** TC2
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
095500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030721     C*** SCL
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
095900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030721     C*** RMO
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
096300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030721     C*** NMO
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
096700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
096800030721     C                   ENDIF
096900040119     C*** CBO
097000040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
097100040706     C                   EVAL      VABCBO = SkSplitCSV(i)
097200040119     C                   ENDIF
097300030721     C*
097400040706     C* Reperisco quindi i campi numerici...
097500040706     C*** NSP
097600040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
097700040706     C                   EVAL      PiStr=SkSplitCSV(i)
097800040706     C                   EXSR      CHKNUM
097900040706     C                   IF        PiInt=*on
098000040706     C                   Z-ADD     PiVal         VABNSP
098100040706     C                   ELSE
098200040706     C                   SETON                                        31
098300040706     C                   EVAL      VABNSP = *zeros
098400040706     C                   EVAL      vinmsg = %trimr(vinmsg)
098500040706     C                             + ' ' + 'VABNSP'
098600040706     C                   ENDIF
098700040706     C                   ENDIF
098800070208     C*** RMN
098900040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
099000060530     C                   EVAL      PiStr=%subst(SkSplitCSV(i):2)
099100040706     C                   EXSR      CHKNUM
099200040706     C                   IF        PiInt=*on
099300040706     C                   Z-ADD     PiVal         VABRMN
099400040706     C                   ELSE
099500040706     C                   SETON                                        32
099600040706     C                   EVAL      VABRMN = 1
099700040706     C                   EVAL      vinmsg = %trimr(vinmsg)
099800060530     C                             + ' ' + 'VABRMN'
099900040706     C                   ENDIF
100000040706     C                   ENDIF
100100030721     C*** CAD
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
100300030721     C                   EVAL      PiStr=SkSplitCSV(i)
100400030721     C                   EXSR      CHKNUM
100500030721     C                   IF        PiInt=*on
100600030721     C                   Z-ADD     PiVal         Num5_0
100700030721     C                   MOVEL(p)  Num5_0        VABCAD
100800030721     C                   ELSE
100900030721     C                   SETON                                        32
101000030721     C                   EVAL      VABCAD = *zeros
101100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101200030721     C                             + ' ' + 'VABCAD'
101300030721     C                   ENDIF
101400030721     C                   ENDIF
101500030721     C*** NCL
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
101700030721     C                   EVAL      PiStr=SkSplitCSV(i)
101800030721     C                   EXSR      CHKNUM
101900030721     C                   IF        PiInt=*on
102000030721     C                   Z-ADD     PiVal         VABNCL
102100030721     C                   ELSE
102200030721     C                   SETON                                        32
102300030721     C                   EVAL      VABNCL = *zeros
102400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102500040706     C                             + ' ' + 'VABNCL'
102600030721     C                   ENDIF
102700030721     C                   ENDIF
102800030721     C*** PKB
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
103000030721     C                   EVAL      PiStr=SkSplitCSV(i)
103100030721     C                   EXSR      CHKNUM
103200030721     C                   IF        PiNum=*on
103300050330     C                   Z-ADD(h)  PiVal         VABPKB
103400030721     C                   ELSE
103500030721     C                   SETON                                        32
103600030721     C                   EVAL      VABPKB = *zeros
103700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103800030721     C                             + ' ' + 'VABPKB'
103900030721     C                   ENDIF
104000030721     C                   ENDIF
104100030721     C*** CAS
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
104300030721     C                   EVAL      PiStr=SkSplitCSV(i)
104400030721     C                   EXSR      CHKNUM
104500030721     C                   IF        PiNum=*on
104600030721     C                   Z-ADD     PiVal         VABCAS
104700030721     C                   ELSE
104800030721     C                   SETON                                        32
104900030721     C                   EVAL      VABCAS = *zeros
105000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105100030721     C                             + ' ' + 'VABCAS'
105200030721     C                   ENDIF
105300030721     C                   ENDIF
105400030721     C*** CCM
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
105600030721     C                   EVAL      PiStr=SkSplitCSV(i)
105700030721     C                   EXSR      CHKNUM
105800030721     C                   IF        PiInt=*on      AND
105900030721     C                             PiVal<=9999999 AND
106000030721     C                             PiVal>*zeros
106100030721     C                   Z-ADD     PiVal         VABCCM
106200070208     C   60              Z-ADD     PiVal         VATCCM
106300030721     C                   ELSE
106400030721     C                   SETON                                        32
106500030721     C                   EVAL      VABCCM = *zeros
106600070208     C  N60              EVAL      vinmsg = %trimr(vinmsg)
106700070208     C                             + ' ' + 'VABCCM'
106800070208     C   60              EVAL      vinmsg = %trimr(vinmsg)
106900070208     C                             + ' ' + 'VABCCM VATCCM'
107000030721     C                   ENDIF
107100030721     C                   ENDIF
107200030721     C*** LNP
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
107400030721     C                   EVAL      PiStr=SkSplitCSV(i)
107500030721     C                   EXSR      CHKNUM
107600030721     C                   IF        PiInt=*on    AND
107700030721     C                             PiVal<=999   AND
107800030721     C                             PiVal>*zeros
107900030721     C                   Z-ADD     PiVal         VABLNP
108000070208     C   60              Z-ADD     PiVal         VATLNP
108100030721     C                   ELSE
108200030721     C                   SETON                                        32
108300030721     C                   EVAL      VABLNP = *zeros
108400070208     C  N60              EVAL      vinmsg = %trimr(vinmsg)
108500070208     C                             + ' ' + 'VABLNP'
108600070208     C   60              EVAL      vinmsg = %trimr(vinmsg)
108700070208     C                             + ' ' + 'VABLNP VATLNP'
108800030721     C                   ENDIF
108900030721     C                   ENDIF
109000030721     C*** NRS
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
109200030721     C                   EVAL      PiStr=SkSplitCSV(i)
109300030721     C                   EXSR      CHKNUM
109400030721     C                   IF        PiInt=*on AND
109500030721     C                             PiVal<=99
109600030721     C                   Z-ADD     PiVal         VABNRS
109700030721     C                   ELSE
109800030721     C                   SETON                                        32
109900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110000030721     C                             + ' ' + 'VABNRS'
110100030721     C                   ENDIF
110200030721     C                   ENDIF
110300030721     C*** LNA
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
110500030721     C                   EVAL      PiStr=SkSplitCSV(i)
110600030721     C                   EXSR      CHKNUM
110700030721     C                   IF        PiInt=*on    AND
110800030721     C                             PiVal<=999   AND
110900030721     C                             PiVal>*zeros
111000030721     C                   Z-ADD     PiVal         VABLNA
111100030721     C                   ELSE
111200030721     C                   SETON                                        32
111300030721     C                   EVAL      VABLNA = *zeros
111400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111500030721     C                             + ' ' + 'VABLNA'
111600030721     C                   ENDIF
111700030721     C                   ENDIF
111800030721     C*** CTR
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiInt=*on  AND
112300030721     C                             PiVal<=999
112400030721     C                   Z-ADD     PiVal         VABCTR
112500030721     C                   ELSE
112600030721     C                   SETON                                        32
112700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112800030721     C                             + ' ' + 'VABCTR'
112900030721     C                   ENDIF
113000030721     C                   ENDIF
113100030721     C*** IAS
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
113300030721     C                   EVAL      PiStr=SkSplitCSV(i)
113400030721     C                   EXSR      CHKNUM
113500030721     C                   IF        PiNum=*on
113600030721     C                   Z-ADD     PiVal         VABIAS
113700030721     C                   ELSE
113800030721     C                   SETON                                        32
113900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114000030721     C                             + ' ' + 'VABIAS'
114100030721     C                   ENDIF
114200030721     C                   ENDIF
114300030721     C*** VLB
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
114500030721     C                   EVAL      PiStr=SkSplitCSV(i)
114600030721     C                   EXSR      CHKNUM
114700030721     C                   IF        PiNum=*on
114800030721     C                   Z-ADD     PiVal         VABVLB
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200030721     C                             + ' ' + 'VABVLB'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** QFT
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiNum=*on
116000030721     C                   Z-ADD     PiVal         VABQFT
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400030721     C                             + ' ' + 'VABQFT'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** NCD
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiInt=*on      AND
117200030721     C                             PiVal<=9999999
117300030721     C                   Z-ADD     PiVal         VABNCD
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117700030721     C                             + ' ' + 'VABNCD'
117800030721     C                   ENDIF
117900030721     C                   ENDIF
118000030721     C*** NCA
118100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
118200030721     C                   EVAL      PiStr=SkSplitCSV(i)
118300030721     C                   EXSR      CHKNUM
118400030721     C                   IF        PiInt=*on      AND
118500030721     C                             PiVal<=9999999
118600030721     C                   Z-ADD     PiVal         VABNCA
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119000030721     C                             + ' ' + 'VABNCA'
119100030721     C                   ENDIF
119200030721     C                   ENDIF
119300030721     C*** ZNC
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
119500030721     C                   EVAL      PiStr=SkSplitCSV(i)
119600030721     C                   EXSR      CHKNUM
119700030721     C                   IF        PiInt=*on AND
119800030721     C                             PiVal<=99
119900030721     C                   Z-ADD     PiVal         VABZNC
120000030721     C                   ELSE
120100030721     C                   SETON                                        32
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300030721     C                             + ' ' + 'VABZNC'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** DCR
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiInt=*on       AND
121100030721     C                             PiVal<=99999999
121200030721     C                   Z-ADD     PiVal         VABDCR
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600030721     C                             + ' ' + 'VABDCR'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** HCR
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on   AND
122400030721     C                             PiVal<=9999
122500030721     C                   Z-ADD     PiVal         VABHCR
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABHCR'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** VMD
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiNum=*on
123700030721     C                   Z-ADD     PiVal         VABVMD
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100030721     C                             + ' ' + 'VABVMD'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** ANT
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiInt=*on        AND
124900030721     C                             PiVal<=999999999
125000030721     C                   Z-ADD     PiVal         VABANT
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABANT'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** CMO
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on
126200030721     C                   Z-ADD     PiVal         Num5_0
126300030721     C                   MOVEL(p)  Num5_0        VABCMO
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      VABCMO = *zeros
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABCMO'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100040706     C*
127200040706     C* Gestisco i campi "personalizzati"
127300030715     C*
127400030715     C                   ENDIF
127500030715     C                   EVAL      i = i + 1
127600030715     C                   ENDDO
127700040706     C*
127800040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
127900040706     C* Reperisco la provincia dal CAP e dalla località
128000040706     C                   IF        VABCAD <> *blanks
128100040706     C                   CLEAR                   TISI95DS
128200040706     C                   EVAL      I95TCN = '3'
128300040706     C                   Z-ADD     datcor        I95DAT
128400040706     C                   EVAL      I95CAP = VABCAD
128500040706     C                   EVAL      I95LOC = VABLOD
128600040706     C                   CALL      'TISI95R'
128700040706     C                   PARM                    TISI95DS
128800040706     C                   EVAL      VABPRD = O95PRV
128900040706     C                   ENDIF
129000070208     C*
129100060530     C* NSP => Stacco un numeratore da AZNUM se NSP a 0
129200070208     C                   IF        VABNSP = 0
129300060530     C                   clear                   TRUL33DS
129400060530     C                   eval      I33OPE = *zeros
129500060530     C                   eval      I33CNU = 302
129600060530     C                   eval      I33NUM = 1
129700060530     C                   movel     TRUL33DS      KPJBU
129800060530     C                   call      'TRUL33R'
129900060530     C                   parm                    KPJBA
130000060530     C                   movel     KPJBU         TRUL33DS
130100060530     C                   if        O33ERR = *zeros
130200060530     C                   z-add     O33NRF        VABNSP
130300070208     C   60              z-add     O33NRF        VATNSP
130400060530     C                   else
130500070208     C  N60              EVAL      vinmsg = %trimr(vinmsg)
130600070208     C                             + ' ' + 'VABNSP'
130700070208     C   60              EVAL      vinmsg = %trimr(vinmsg)
130800070208     C                             + ' ' + 'VABNSP VATNSP'
130900060530     C                   endif
131000060530     C                   ENDIF
131100040706     C*
131200040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
131300040706     C* d valorizzarli sempre entrambi
131400040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
131500040706     C                   Z-ADD     VABNSP        VABRMN
131600040706     C                   ENDIF
131700040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
131800040706     C                   MOVEL     VABRMN        VABRMA
131900040706     C                   ENDIF
132000040113     C*
132100040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
132200040113     C                   IF        VABCAS > *zeros
132300060327     C                   EVAL      VABVCA = 'EUR'
132400040119     C                   IF        VABCBO = '1'
132500040113     C                   EVAL      VABCBO = '4'
132600040119     C                   ENDIF
132700040119     C                   IF        VABCBO = '2'
132800040119     C                   EVAL      VABCBO = '6'
132900040119     C                   ENDIF
133000040113     C                   ENDIF
133100020204     C*
133200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
133300020204     C                   EXSR      CHKIMPDIV
133400030715     C                   ENDIF
133500020204     C*
133600010607     C                   ENDSR
133700010607     C*----------------------------------------------------*
133800010601
133900020204
134000020204     C*----------------------------------------------------*
134100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
134200020204     C*----------------------------------------------------*
134300020204     C     CHKIMPDIV     BEGSR
134400020204     C*
134500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
134600020204     C                   Z-ADD     *zeros        wrkDec            9 9
134700020204     C*
134800020204     C* Come prima cosa effettuo considerazioni sulla divisa
134900020204     C                   IF        vabIAS > *zeros
135000020204     C                   IF        vabVAS <> 'EUR'
135100020204     C                   EVAL      vabVAS =  'ITL'
135200020204     C                   ENDIF
135300020204     C                   ENDIF
135400020204     C*
135500020204     C                   IF        vabCAS > *zeros
135600020204     C                   IF        vabVCA <> 'EUR'
135700020204     C                   EVAL      vabVCA =  'ITL'
135800020204     C                   ENDIF
135900020204     C                   ENDIF
136000020204     C*
136100020204     C                   IF        vabVMD > *zeros
136200020204     C                   IF        vabVAD <> 'EUR'
136300020204     C                   EVAL      vabVAD =  'ITL'
136400020204     C                   ENDIF
136500020204     C                   ENDIF
136600020204     C*
136700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
136800020204     C                   Z-ADD     vabIAS        wrkDec
136900020204     C                   IF        wrkDec > *zeros
137000020204     C                   IF        vabVAS = 'ITL'
137100020204     C                   EVAL      vabIAS = *zeros
137200020204     C                   ENDIF
137300020204     C                   ENDIF
137400020204     C*
137500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
137600020204     C                   Z-ADD     vabCAS        wrkDec
137700020204     C                   IF        wrkDec > *zeros
137800020204     C                   IF        vabVCA = 'ITL'
137900020204     C                   EVAL      vabCAS = *zeros
138000020204     C                   ENDIF
138100020204     C                   ENDIF
138200020204     C*
138300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
138400020204     C                   Z-ADD     vabVMD        wrkDec
138500020204     C                   IF        wrkDec > *zeros
138600020204     C                   IF        vabVAD = 'ITL'
138700020204     C                   EVAL      vabVMD = *zeros
138800020204     C                   ENDIF
138900020204     C                   ENDIF
139000020204     C*
139100020204     C                   ENDSR
139200020204     C***
139300020204
139400010330
139500010330
139600010330     C*----------------------------------------------------*
139700010330     C*  CONTROLLO NUMERICITA' CAMPI
139800010330     C*----------------------------------------------------*
139900010330     C     CHKNUM        BEGSR
140000010330     C*
140100010606     C                   IF        PiDecChr = *blanks
140200030715     C                   EVAL      PiDecChr = CharNUM
140300010606     C                   ENDIF
140400010606     C*
140500010606     C                   CALL(e)   'ISNUMERIC'
140600010330     C                   PARM                    PiStr            30
140700010606     C                   PARM                    PiDecChr          1
140800010330     C                   PARM      *ZEROS        PiVal            30 9
140900010330     C                   PARM      '0'           PiInt             1
141000010330     C                   PARM      '0'           PiNum             1
141100010330     C                   IF        %error
141200010606     C                   EVAL      PiNum=*off
141300010330     C                   ENDIF
141400010330     C*
141500010330     C                   ENDSR
141600010330     C***
141700010330
141800010601
141900010601
142000010601
142100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
142200010601     C     repfil        BEGSR
142300010601     C*
142400010601     C                   if        invfil = *zeros and
142500010601     C                             depfil > *zeros and
142600010629     C                             (vinflg = *blanks or
142700010629     C                              vinflg = *zeros)
142800010601     C
142900010601     C                   eval      invfil = depfil
143000010601     C                   endif
143100010601     C*
143200010601     C                   if        depfil <> invfil and
143300010601     C                             invfil > *zeros
143400010601     C                   eval      flgMulti = '1'
143500010601     C                   if        vinflg = *blanks
143600010601     C                   add       1             cntNonEl
143700010601     C                   endif
143800010601     C                   endif
143900010601     C*
144000010601     C                   if        vinflg = '2'
144100010601     C                   eval      flgStato = '2'
144200010601     C                   endif
144300010601     C*
144400010601     C                   ENDSR
144500010601     C***
144600010601
144700010601
144800010601
144900010330
145000040119      /TITLE Invio dei dati al punto operativo.
145100040119     C     invio         BEGSR
145200040119     C*
145300070301     C* 1° invio EDIVAT
145400070208     C                   if        *in60 = *on
145500040119     C                   reset                   dscmz
145600040119     C                   move      vlrpoi        cmzdst
145700070301     C                   eval      cmzfld = 'EDIVATWR'
145800040119     C                   eval      cmzmbd = vlrhdl
145900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
146000040119     C***                if        prmfir = *blanks
146100070301     C                   eval      cmzfla = 'EDIVAT0F'
146200070301     C                   eval      cmzmba = 'EDIVAT0F'
146300040119     C***                else
146400040119     C***                eval      cmzfla = prmfir
146500040119     C***                eval      cmzmba = prmfir
146600040119     C***                endif
146700040119     C                   eval      cmznrr = *zeros
146800040119     C                   move      §ctrokvt      cmznrr
146900040119     C                   eval      cmzlba = vlrfl1
147000040119     C                   call(e)   'TIS711C'
147100040119     C                   parm                    dscmz
147200040119     C                   parm      *blanks       esito
147300070208     C                   endif
147400040119     C                   if        %error
147500040119     C                             or cmzerr = '1'
147600040119     C                             or esito  = '1'
147700040119     C                   eval      wrkesito = '3'
147800040119     C                   else
147900040119     C*
148000070301     C* 2° invio EDIVAB
148100040119     C                   reset                   dscmz
148200040119     C                   move      vlrpoi        cmzdst
148300040119     C                   eval      cmzfld = vlrfou
148400040119     C                   eval      cmzmbd = vlrhdl
148500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
148600040119     C***                if        prmfir = *blanks
148700070301     C                   eval      cmzfla = 'EDIVAB0F'
148800070301     C                   eval      cmzmba = 'EDIVAB0F'
148900040119     C***                else
149000040119     C***                eval      cmzfla = prmfir
149100040119     C***                eval      cmzmba = prmfir
149200040119     C***                endif
149300040119     C                   eval      cmznrr = *zeros
149400040119     C                   move      §ctrokvb      cmznrr
149500040119     C                   eval      cmzlba = vlrfl1
149600040119     C                   call(e)   'TIS711C'
149700040119     C                   parm                    dscmz
149800040119     C                   parm      *blanks       esito
149900040119     C                   if        %error
150000040119     C                             or cmzerr = '1'
150100040119     C                             or esito  = '1'
150200040119     C                   eval      wrkesito = '3'
150300040119     C                   endif
150400040119     C                   endif
150500040119     C*
150600040119     C                   ENDSR
150700040119     C***
150800010601
150900010601
151000010601
151100010601
151200010601      /TITLE Invio dei dati al punto operativo.
151300010601     C     opeini        BEGSR
151400010601     C*
151500010601     C* Inizializzo flag e contatori operativi
151600010601     C                   movel     '0'           flgGiro           1
151700010601     C                   movel     '0'           flgMulti          1
151800010601     C                   movel     '1'           flgStato          1
151900010615     C                   movel     '0'           flgOk             1
152000010601     C                   z-add     *zeros        cntNonEl         10 0
152100010601     C                   z-add     *zeros        depfil            3 0
152200010601     C                   z-add     *zeros        invfil            3 0
152300010601     C*
152400010601     C                   ENDSR
152500010601     C***
152600010601
152700010601
152800050421
152900050421
153000050421     C     *pssr         BEGSR
153100050421     C*
153200050421     C                   eval      wrkesito = '2'
153300050421     C*
153400050421     C                   ENDSR     '*CANCL'
153500050421     C***
153600050421
153700010601
153800010330
153900010330
154000000613     C     *inzsr        BEGSR
154100990910     C*
154200990910     C     *entry        plist
154300990920     C                   parm                    tivlrds
154400990921     C                   parm      wrkesito      esito
154500000724     C                   parm                    prmlit
154600000710     C                   parm                    prmfir
154700070306     C*
154800070306     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
154900070306     C                   move(p)   vlrMSG        wNomeFile
155000070306     C                   if        wNomeFile <> *blanks
155100070306     C                   eval      wNomeFile = %subst(wNomeFile:15:6)
155200070306     C                   else
155300070306     C                   eval      wNomeFile = 'CECCATO'
155400070306     C                   endif
155500010330     C*
155600010330     C* CALCOLA LA DATA CORRENTE
155700010330     C                   time                    wn14             14 0
155800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
155900070301     C                   movel     wn14          oracor            6 0          *ORA
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
157000070216VABRMN~X02~X03~X04~X05~X06~X07~X08~VABRMA~VABNAS~VABPKB~X12~
157100070216X13~X14~X15~X16~X17~X18~X19~X20~VABRSD~VABRD2~VABIND~VABLOD~
157200070216X25~X26~VABCAD~X28~X29~X30~X31~X32~X33~X34~X35
