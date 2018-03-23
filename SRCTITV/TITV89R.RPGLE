000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600040119     FFIVATwwr  O    E             DISK    usropn
000700040113     D*------------
000800040113     D* SCHIEDE A PROGRAMMA
000900040113     D*------------
001000040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D dscmz         e ds                  inz
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800010330     D esito           s              1
001900010330     D prmlit          s             10
002000010330     D prmfir          s             10
002100010330     D wrkesito        s                   like(esito)
002200010330     D rrnum           s              6  0 INZ(*zeros)
002300040119     D wAlfa2          s              2    INZ(*blanks)
002400040119     D wAlfa4          s              4    INZ(*blanks)
002500121221     D wNumCollo       s              5  0 INZ(*zeros)
002600040119     D wNumColloA      s              2    INZ(*blanks)
002700040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
002800040119     D depnot          s                   LIKE(VATNOT) INZ(*blanks)
002900040119     D parccm          s              8    INZ(*blanks)
003000040119     D parmbr          s             10    INZ(*blanks)
003100040119     D paropz          s              1    INZ(*blanks)
003200040119     D chkcall         s              1    INZ(*blanks)
003300030715     D*------------------
003400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003500030715     D*------------------
003600030715     D SkSplitFLD      S             10    DIM(100)
003700030715     D SkSplitCSV      S            256    DIM(100)
003800030715     D CharCSV         S              1
003900030715     D CharTXT         S              1
004000030715     D CharNUM         S              1
004100030715     D posDa           S              3  0 INZ(*zeros)
004200030715     D posA            S              3  0 INZ(*zeros)
004300030715     D i               s              3  0 INZ(1)
004400030715     D wGiro           s              1  0 INZ(*zeros)
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
010600040119     C                   parm                    parccm
010700040119     C                   parm                    parmbr
010800040119     C                   parm      '2'           paropz
010900000616     C*
011000010601     C                   ENDSR
011100010601     C***
011200010601
011300010330     C*--------------------------------------------------------
011400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
011500010330     C*--------------------------------------------------------
011600010605     C     RWVAB         BEGSR
011700010330     C*
011800010330     C                   if        not %open(tivin00r)
011900010330     C                   open      tivin00r
012000010330     C                   endif
012100021025     C                   if        not %open(fivabwwr)
012200021025     C                   open      fivabwwr
012300010330     C                   endif
012400040119     C*
012500040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
012600040119     C                   exsr      prevat
012700040119     C*
012800040119     C                   if        chkcall = '0'
012900040119     C*
013000040119     C                   if        not %open(fivatwwr)
013100040119     C                   open      fivatwwr
013200040119     C                   endif
013300010330     C*
013400040119     C                   clear                   §CTROKVB          7 0
013500040119     C                   clear                   §CTROKVT          7 0
013600040119     C                   clear                   §CTRMO            7 0
013700040119     C                   clear                   §CTRNO            7 0
013800010330     C*
013900030822     C                   do        *HIVAL
014000010330     C*
014100030822     C                   read      tivin00r                               70
014200010618     C*
014300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014400010618     C                   if        vinflg = '1'
014500010618     C                   eval      flgOk = '1'
014600010618     C                   endif
014700010618     C*
014800040316     C                   if        (vindta > *blanks AND
014900040316     C                              %trim(vindta) <> *all';') OR
015000040316     C                             wGiro = *zeros
015100010330     C                   add       1             rrnum
015200010330     C*
015300010601     C                   if        *in70 = *off and
015400010330     C                             (vinflg = *blanks
015500010330     C                              or vinflg = '0'
015600010330     C                              or vinflg = '2')
015700010330     C*
015800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015900010711     C                   if        vinflg = *blanks or vinflg = '0'
016000010711     C                   clear                   vinmsg
016100010711     C                   endif
016200010601     C*
016300010330     C                   exsr      inzvar
016400010605     C                   exsr      impvab
016500010601     C*
016600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
016700010605     C                   eval      depfil = VABLNP
016800010601     C                   exsr      repfil
016900010601     C                   if        depfil = invfil
017000021025     C                   if        vlrpoi = 999
017100030822     C                   move(P)   invfil        VABFGS
017200040119     C                   move(P)   invfil        VATFGS
017300021025     C                   else
017400030822     C                   move(P)   vlrpoi        VABFGS
017500040119     C                   move(P)   vlrpoi        VATFGS
017600021025     C                   endif
017700010601     C*
017800010601     C                   exsr      PREELA
017900010601     C*
018000010604     C* Ebbene...
018100010604     C*
018200040119     C  N31              add       1             §CTROKVB
018300040119     C   32              add       1             §CTRMO
018400040119     C   31              add       1             §CTRNO
018500030822     C                   if        wGiro = 2
018600040119     C                   if        depspe =  *zeros OR
018700040119     C                             depspe <> vabNSP
018800040119     C  N31              exsr      wrivab
018900040119     C  N31              eval      depspe = vabNSP
019000040119     C                   endif
019100040119     C                   endif
019200010604     C*
019300010604     C                   if        *in31 = *off and
019400010604     C                             *in32 = *off
019500010604     C                   eval      vinflg = '1'
019600010604     C                   else
019700010604     C                   eval      vinflg = '2'
019800010604     C                   endif
019900010604     C                   endif
020000010601     C*
020100010604     C                   endif
020200010604     C*
020300010330     C                   else
020400010330     C                   eval      vinflg = '1'
020500010330     C                   endif
020600010601     C*
020700010601     C  N70              update    tivin000
020800010330     C*
020900030822     C  N70              enddo
021000040119     C*
021100040119     C                   endif                                                  (endif - chkcall)
021200010601     C*
021300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
021400010601     C                   if        cntNonEl = *zeros or
021500010601     C                             flgMulti = '0'
021600010330     C* Se non ci sono record con errori ...
021700010601     C                   if        §ctrno = 0 and
021800010604     C                             §ctrmo = 0 and
021900010601     C                             flgStato <> '2'
022000010330     C* ... restituisco esito OK.
022100010330     C                   eval      wrkesito = '0'
022200010330     C                   else
022300040119     C                   if        §ctrokvb > 0
022400010330     C                   eval      wrkesito = '1'
022500010330     C                   else
022600010615     C                   if        flgOk = '0'
022700010615     C                   eval      wrkesito = '2'
022800010615     C                   else
022900010615     C                   eval      wrkesito = '6'
023000010615     C                   endif
023100010330     C                   endif
023200010330     C                   endif
023300010601     C                   else
023400010601     C                   eval      wrkesito = '9'
023500010601     C                   endif
023600010330     C*
023700010330     C                   if        %open(tivin00r)
023800010330     C                   close     tivin00r
023900010330     C                   endif
024000021025     C                   if        %open(fivabwwr)
024100021025     C                   close     fivabwwr
024200010330     C                   endif
024300040512     C                   if        %open(fivatwwr)
024400040512     C                   close     fivatwwr
024500040512     C                   endif
024600010601     C*
024700010601     C                   if        vlrpoi <> 999
024800010601     C                   eval      invfil = vlrpoi
024900010601     C                   endif
025000010330     C*
025100040119     C                   if        §ctrokvb > 0
025200010601     C                             and invfil > *zeros
025300010330     C                   exsr      invio
025400010330     C                   endif
025500010601     C*
025600010618     C                   if        flgGiro = '1'
025700010601     C                   exsr      endela
025800010618     C                   endif
025900010330     C*
026000010330     C                   ENDSR
026100010330     C***
026200040119
026300040119
026400040119
026500040119     C*----------------------------------------------------*
026600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
026700040119     C*----------------------------------------------------*
026800040119     C     WRIVAB        BEGSR
026900040119     C*
027000040119     C                   write     fivab000                                     => scarico il VAB
027100040119     C*
027200041008     C* Se trattasi d "Disk C" eseguo la scrittura dei "CHI SONO" della spedizione corrente
027300041008     C                   if        %subst(VABCTM:1:1) = '7'
027400040119     C                   exsr      wrivat
027500041008     C                   endif
027600040119     C*
027700040119     C                   ENDSR
027800040119
027900040119
028000040119
028100040119     C*----------------------------------------------------*
028200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
028300040119     C*----------------------------------------------------*
028400040119     C     WRIVAT        BEGSR
028500040119     C*
028600040119     C* Inizializzo progressivo collo su totale colli della spedizione
028700040119     C                   Z-ADD     *zeros        wNumCollo
028800040119     C*
028900040119     C* Salvo la parte costante del barcode relativa alla spedizoine corrente
029000040119     C                   EVAL      depnot = VATNOT
029100040119     C*
029200040119     C* Costriuisco <N> barcode a seconda del numero d colli presenti sulla bolla
029300040119     C                   DOW       wNumCollo < VABNCL
029400040119     C*** 4° parte VATNOT
029500040119     C                   EVAL      VATNOT = depnot
029600040119     C                   EVAL      wNumCollo = wNumCollo + 1
029700040119     C                   MOVE      wNumCollo     wNumColloA
029800040119     C                   EVAL      VATNOT = %trim(VATNOT)+wNumColloA
029900040119     C                   MOVE      VABNCL        wAlfa2
030000040119     C                   EVAL      VATNOT = %trim(VATNOT)+wAlfa2
030100040119     C*
030200040119     C                   write     FIVAT000
030300040119     C                   add       1             §CTROKVT
030400040119     C                   ENDDO
030500040119     C*
030600040119     C                   ENDSR
030700040119
030800040119
030900040119
031000040119     C*----------------------------------------------------*
031100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
031200040119     C*----------------------------------------------------*
031300040119     C     PREVAT        BEGSR
031400040119     C*
031500040119     C* Compongo il nome del membro da dare al FIVATWWR
031600040119     C                   eval      parmbr = vlrhdl
031700040119     C                   movel     'M'           parmbr
031800041008     C                   eval      parccm = vlrksc
031900040119     C                   eval      paropz = '1'
032000040119     C* Effettuo la chiamata al CLLE preposto
032100040119     C                   call(e)   'TITVVTC'
032200040119     C                   parm                    parccm
032300040119     C                   parm                    parmbr
032400040119     C                   parm                    paropz
032500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
032600040119     C                   if        %error
032700040119     C                   movel     '1'           chkcall
032800040119     C                   else
032900040119     C                   movel     '0'           chkcall
033000040119     C                   endif
033100040119     C*
033200040119     C                   ENDSR
033300010601
033400010601
033500010601
033600010330     C*----------------------------------------------------*
033700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033800010330     C*----------------------------------------------------*
033900040119     C     INZVAR        BEGSR
034000010330     C*
034100030822     C* Inizializzo variabili di wrk
034200020204     C                   Z-ADD     *zeros        Num5_0            5 0
034300040119     C                   MOVEL     *blanks       wAlfa2
034400040119     C                   MOVEL     *blanks       wAlfa4
034500030822     C*
034600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
034700030822     C                   CLEAR                   FIVAB000
034800040119     C                   CLEAR                   FIVAT000
034900030822     C                   CLEAR                   SkSplitCSV
035000030822     C                   EVAL      i = 1
035100030822     C                   EVAL      posDa = *zeros
035200030822     C                   EVAL      posA  = *zeros
035300030822     C*
035400030822     C* Reimposto i valori di default
035500030822     C                   EXSR      DEFCAM
035600030822     C*
035700010330     C                   ENDSR
035800010330     C*----------------------------------------------------*
035900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
036000010330     C*----------------------------------------------------*
036100010330     C     DEFCAM        BEGSR
036200040119     C*
036300040119     C* Imposto i valori d default a programma dei campi fondamentali
036400040119     C                   EVAL      VABCCM = 1200594
036500040119     C                   EVAL      VATCCM = 1200594
036600040119     C                   EVAL      VABLNP = 120
036700040119     C                   EVAL      VATLNP = 120
036800040119     C                   EVAL      VABCTR = 005
036900041008     C***                EVAL      VABCTM = '7Q'
037000040119     C                   EVAL      VABCBO = '1'
037100040119     C                   EVAL      VATTRC = 'E'
037200030822     C*
037300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
037400030715     C* e delimitatore testo.
037500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
037600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
037700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
037800030822     C*
037900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
038000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
038100030715     C                   SETON                                        50
038200030715     C                   ELSE
038300030715     C                   SETOFF                                       50
038400030715     C                   ENDIF
038500030822     C*
038600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
038700030822     C                   EVAL      posDaDft = 1
038800030822     C                   EVAL      posADft  = 0
038900030822     C                   EVAL      wGiroDft = 0
039000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
039100030822     C                             posDaDft > 0
039200030822     C*
039300030822     C* Gestisco il 1° giro
039400030822     C                   IF        wGiroDft = 0
039500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
039600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
039700030822     C* Incremento il contatore dei "giri"
039800030822     C                   EVAL      wGiroDft = 1
039900030822     C                   ELSE
040000030822     C                   EVAL      posDaDft = posADft
040100030822     C                   ENDIF
040200030822     C* Eseguo lo scan x trovare la fine del campo corrente
040300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
040400030822     C*
040500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
040600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
040700030822     C                   IF        posDaDft > 0 AND
040800030822     C                             posADft  > 0
040900030822     C* NCL
041000030822     C                   IF        %subst(
041100030822     C                             %subst(vlrppt:posDaDft+1:
041200030822     C                             posADft-posDaDft-1):1:3)
041300030822     C                             = 'NCL'
041400030822     C                   EVAL      PiStr=%trim(%subst(
041500030822     C                             %subst(vlrppt:posDaDft+1:
041600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041700030822     C                   EXSR      CHKNUM
041800030822     C                   IF        PiInt=*on
041900030822     C                   Z-ADD     PiVal         VABNCL
042000030822     C                   ENDIF
042100030822     C                   ENDIF
042200030822     C* CCM
042300030822     C                   IF        %subst(
042400030822     C                             %subst(vlrppt:posDaDft+1:
042500030822     C                             posADft-posDaDft-1):1:3)
042600030822     C                             = 'CCM'
042700030822     C                   EVAL      PiStr=%trim(%subst(
042800030822     C                             %subst(vlrppt:posDaDft+1:
042900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043000030822     C                   EXSR      CHKNUM
043100030822     C                   IF        PiInt=*on
043200030822     C                   Z-ADD     PiVal         VABCCM
043300040119     C                   Z-ADD     PiVal         VATCCM
043400030822     C                   ENDIF
043500030822     C                   ENDIF
043600030822     C* LNP
043700030822     C                   IF        %subst(
043800030822     C                             %subst(vlrppt:posDaDft+1:
043900030822     C                             posADft-posDaDft-1):1:3)
044000030822     C                             = 'LNP'
044100030822     C                   EVAL      PiStr=%trim(%subst(
044200030822     C                             %subst(vlrppt:posDaDft+1:
044300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044400030822     C                   EXSR      CHKNUM
044500030822     C                   IF        PiInt=*on
044600030822     C                   Z-ADD     PiVal         VABLNP
044700040119     C                   Z-ADD     PiVal         VATLNP
044800030822     C                   ENDIF
044900030822     C                   ENDIF
045000030822     C* NRS
045100030822     C                   IF        %subst(
045200030822     C                             %subst(vlrppt:posDaDft+1:
045300030822     C                             posADft-posDaDft-1):1:3)
045400030822     C                             = 'NRS'
045500030822     C                   EVAL      PiStr=%trim(%subst(
045600030822     C                             %subst(vlrppt:posDaDft+1:
045700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045800030822     C                   EXSR      CHKNUM
045900030822     C                   IF        PiInt=*on
046000030822     C                   Z-ADD     PiVal         VABNRS
046100030822     C                   ENDIF
046200030822     C                   ENDIF
046300030822     C* CTR
046400030822     C                   IF        %subst(
046500030822     C                             %subst(vlrppt:posDaDft+1:
046600030822     C                             posADft-posDaDft-1):1:3)
046700030822     C                             = 'CTR'
046800030822     C                   EVAL      PiStr=%trim(%subst(
046900030822     C                             %subst(vlrppt:posDaDft+1:
047000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047100030822     C                   EXSR      CHKNUM
047200030822     C                   IF        PiInt=*on
047300030822     C                   Z-ADD     PiVal         VABCTR
047400030822     C                   ENDIF
047500030822     C                   ENDIF
047600030822     C* PKB
047700030822     C                   IF        %subst(
047800030822     C                             %subst(vlrppt:posDaDft+1:
047900030822     C                             posADft-posDaDft-1):1:3)
048000030822     C                             = 'PKB'
048100030822     C                   EVAL      PiStr=%trim(%subst(
048200030822     C                             %subst(vlrppt:posDaDft+1:
048300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048400030822     C                   EXSR      CHKNUM
048500030822     C                   IF        PiNum=*on
048600030822     C                   Z-ADD     PiVal         VABPKB
048700030822     C                   ENDIF
048800030822     C                   ENDIF
048900030822     C* VLB
049000030822     C                   IF        %subst(
049100030822     C                             %subst(vlrppt:posDaDft+1:
049200030822     C                             posADft-posDaDft-1):1:3)
049300030822     C                             = 'VLB'
049400030822     C                   EVAL      PiStr=%trim(%subst(
049500030822     C                             %subst(vlrppt:posDaDft+1:
049600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049700030822     C                   EXSR      CHKNUM
049800030822     C                   IF        PiNum=*on
049900030822     C                   Z-ADD     PiVal         VABVLB
050000030822     C                   ENDIF
050100030822     C                   ENDIF
050200030822     C* QFT
050300030822     C                   IF        %subst(
050400030822     C                             %subst(vlrppt:posDaDft+1:
050500030822     C                             posADft-posDaDft-1):1:3)
050600030822     C                             = 'QFT'
050700030822     C                   EVAL      PiStr=%trim(%subst(
050800030822     C                             %subst(vlrppt:posDaDft+1:
050900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051000030822     C                   EXSR      CHKNUM
051100030822     C                   IF        PiNum=*on
051200030822     C                   Z-ADD     PiVal         VABQFT
051300030822     C                   ENDIF
051400030822     C                   ENDIF
051500030822     C* CBO
051600030822     C                   IF        %subst(
051700030822     C                             %subst(vlrppt:posDaDft+1:
051800030822     C                             posADft-posDaDft-1):1:3)
051900030822     C                             = 'CBO'
052000030822     C                   EVAL      VABCBO=%trim(%subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052300030822     C                   ENDIF
052400030822     C* TSP
052500030822     C                   IF        %subst(
052600030822     C                             %subst(vlrppt:posDaDft+1:
052700030822     C                             posADft-posDaDft-1):1:3)
052800030822     C                             = 'TSP'
052900030822     C                   EVAL      VABTSP=%trim(%subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053200030822     C                   ENDIF
053300030822     C* VAS
053400030822     C                   IF        %subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):1:3)
053700030822     C                             = 'VAS'
053800030822     C                   EVAL      VABVAS=%trim(%subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054100030822     C                   ENDIF
054200030822     C* VCA
054300030822     C                   IF        %subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):1:3)
054600030822     C                             = 'VCA'
054700030822     C                   EVAL      VABVCA=%trim(%subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000030822     C                   ENDIF
055100030822     C* GCA
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'GCA'
055600030822     C                   EVAL      VABGCA=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   ENDIF
056000030822     C* CTM
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'CTM'
056500030822     C                   EVAL      VABCTM=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   ENDIF
056900030822     C* FFD
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'FFD'
057400030822     C                   EVAL      VABFFD=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   ENDIF
057800030822     C* VAD
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'VAD'
058300030822     C                   EVAL      VABVAD=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   ENDIF
058700030822     C* GMA
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'GMA'
059200030822     C                   EVAL      VABGMA=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   ENDIF
059600030822     C* GGA
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'GGA'
060100030822     C                   EVAL      VABGGA=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   ENDIF
060500030822     C* GVA
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900030822     C                             = 'GVA'
061000030822     C                   EVAL      VABGVA=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   ENDIF
061400030822     C* TC1
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'TC1'
061900030822     C                   EVAL      VABTC1=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   ENDIF
062300030822     C* TC2
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'TC2'
062800030822     C                   EVAL      VABTC2=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   ENDIF
063200030822     C* ...
063300030822     C                   ENDIF
063400030822     C                   ENDDO
063500020204     C*
063600010330     C                   ENDSR
063700010607     C*----------------------------------------------------*
063800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
063900010607     C*----------------------------------------------------*
064000010607     C     IMPVAB        BEGSR
064100010607     C*
064200010607     C                   SETOFF                                       3132
064300030515     C*
064400010607     C                   MOVEL     datcor        VABAAS
064500040119     C                   MOVEL     datcor        VATAAS
064600010607     C                   MOVE      datcor        VABMGS
064700010607     C*
064800030715     C**********
064900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
065000030715     C**********
065100030715     C                   IF        wGiro = *zeros
065200040113     C*
065300040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
065400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
065500040113     C                                      %trim(intcol(3))
065600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
065700031201     C     minu:maiu     XLATE     vindta        vindta
065800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
065900030715     C                   DOW       posDa <= %len(%trim(vindta))
066000030715     C*
066100030715     C* Gestisco il 1° campo
066200030715     C                   IF        i = 1
066300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
066400030715     C                   EVAL      posDa = 1
066500030715     C* Eseguo lo scan x trovare la fine del primo campo
066600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
066700030715     C* A questo punto "estraggo" il campo corrente
066800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
066900030715     C                                                    (posA-posDa))
067000030715     C* X i campi successivi al 1°
067100030715     C                   ELSE
067200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
067300030715     C                   EVAL      posDa = posA + 1
067400030715     C* Eseguo lo scan x trovare la fine del campo corrente
067500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
067600030715     C* Gestisco l'ultimo campo
067700030715     C                   IF        posA = *zeros
067800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
067900030715     C* A questo punto "estraggo" il campo corrente
068000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
068100030715     C                                                    (posA-posDa))
068200030715     C                   LEAVE
068300030715     C                   ELSE
068400030715     C* A questo punto "estraggo" il campo corrente
068500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
068600030715     C                                                    (posA-posDa))
068700030715     C                   ENDIF
068800030715     C                   ENDIF
068900030715     C* Incremento il contatore d campo
069000030715     C                   EVAL      i = i +1
069100030715     C                   ENDDO
069200030715     C*
069300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
069400030715     C* delimitazione testo
069500030715     C                   EVAL      i = 1
069600030715     C                   DOW       i < %elem(SkSplitFLD)
069700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
069800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
069900030715     C                   EVAL      i = i + 1
070000030715     C                   ENDDO
070100030715     C                   EVAL      wGiro = 1
070200030715     C                   ELSE
070300030715     C                   EVAL      wGiro = 2
070400031204     C**********
070500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
070600031204     C* d separatore campo
070700031204     C**********
070800031204     C                   EVAL      vindta = %trim(vindta)
070900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
071000031204     C                   EVAL      vindta = %subst(vindta:2)
071100031204     C                   ENDDO
071200031204     C*
071300031204     C                   Z-ADD     *zeros        lunghInput        4 0
071400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
071500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
071600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
071700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
071800031204     C                   ENDDO
071900030715     C**********
072000030715     C* Effettuo lo split del campo dati d input
072100030715     C**********
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
073200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
073300030715     C                                                    (posA-posDa))
073400030715     C* X i campi successivi al 1°
073500030715     C                   ELSE
073600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
073700030715     C                   EVAL      posDa = posA + 1
073800031204     C* Verifico che nn vi sia il campo nn valorizzato
073900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
074000031204     C* Se campo nn valorizzato skippo al prossimo
074100031204     C                   EVAL      SkSplitCSV(i) = *blanks
074200031204     C                   EVAL      posA  = posA + 1
074300031204     C                   ELSE
074400030715     C* Eseguo lo scan x trovare la fine del campo corrente
074500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074600030715     C* Gestisco l'ultimo campo
074700030715     C                   IF        posA = *zeros
074800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
074900030715     C* A questo punto "estraggo" il campo corrente
075000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
075100030715     C                                                    (posA-posDa))
075200030715     C                   LEAVE
075300030715     C                   ELSE
075400030715     C* A questo punto "estraggo" il campo corrente
075500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
075600030715     C                                                    (posA-posDa))
075700030715     C                   ENDIF
075800031204     C                   ENDIF
075900030715     C                   ENDIF
076000030715     C* Incremento il contatore d campo
076100030715     C                   EVAL      i = i +1
076200030715     C                   ENDDO
076300030715     C*
076400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
076500030715     C* delimitazione testo
076600030715     C                   EVAL      i = 1
076700030715     C                   DOW       i < %elem(SkSplitCSV)
076800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
076900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
077000030715     C                   EVAL      i = i + 1
077100030715     C                   ENDDO
077200030715     C*
077300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
077400030715     C                   EVAL      i = 1
077500030715     C                   DOW       i < %elem(SkSplitFLD)
077600030715     C*
077700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
077800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
077900030715     C                             SkSplitCSV(i) <> *blanks
078000030721     C*** ATB
078100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
078200030721     C                   EVAL      VABATB = SkSplitCSV(i)
078300030721     C                   ENDIF
078400030715     C*** RSD
078500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
078600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
078700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
078800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
078900030715     C* ==
079000030715     C                   ENDIF
079100030721     C*** RD2
079200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
079300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
079400030721     C                   ENDIF
079500030715     C*** IND
079600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
079700030715     C                   EVAL      VABIND = SkSplitCSV(i)
079800030715     C                   ENDIF
079900030715     C*** LOD
080000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
080100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
080200030715     C                   ENDIF
080300030715     C*** PRD
080400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
080500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
080600030715     C                   ENDIF
080700030721     C*** NZD
080800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
080900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
081000030721     C                   ENDIF
081100030715     C*** RMA
081200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
081300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
081400030715     C                   ENDIF
081500030715     C*** NOT
081600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
081700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
081800030715     C                   ENDIF
081900030715     C*** NT2
082000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
082100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
082200030715     C                   ENDIF
082300030715     C*** VCA
082400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
082500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
082600030715     C                   ENDIF
082700030715     C*** VAS
082800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
082900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
083000030715     C                   ENDIF
083100030715     C*** FFD
083200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
083300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
083400030715     C                   ENDIF
083500030721     C*** GC1
083600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
083700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
083800030721     C                   ENDIF
083900030721     C*** GC2
084000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
084100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
084200030721     C                   ENDIF
084300030721     C*** TSP
084400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
084500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
084600030721     C                   ENDIF
084700030721     C*** NAS
084800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
084900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
085000030721     C                   ENDIF
085100030721     C*** TIC
085200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
085300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
085400030721     C                   ENDIF
085500030721     C*** GCA
085600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
085700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
085800030721     C                   ENDIF
085900030721     C*** XCO
086000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
086100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
086200030721     C                   ENDIF
086300030721     C*** CTM
086400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
086500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
086600030721     C                   ENDIF
086700030721     C*** TCR
086800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
086900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
087000030721     C                   ENDIF
087100030721     C*** CTS
087200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
087300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
087400030721     C                   ENDIF
087500030721     C*** FTM
087600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
087700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
087800030721     C                   ENDIF
087900030721     C*** VAD
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
088100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
088200030721     C                   ENDIF
088300030721     C*** GMA
088400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
088500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
088600030721     C                   ENDIF
088700030721     C*** GGA
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
088900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
089000030721     C                   ENDIF
089100030721     C*** GVA
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
089300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
089400030721     C                   ENDIF
089500030721     C*** TC1
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
089700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030721     C*** TC2
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
090100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** SCL
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
090500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** RMO
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
090900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** NMO
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
091300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
091400030721     C                   ENDIF
091500040119     C*
091600040119     C* Verifico i campi "personalizzati"
091700040119     C*** 1° parte RMA
091800040119     C*** 1° parte VATNOT
091900040119     C                   IF        %trim(SkSplitFLD(i)) = 'BCALFA'
092000040119     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
092100040119     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
092200040119     C                   ENDIF
092300040119     C*** RMN
092400040119     C*** 2° parte VATNOT
092500040119     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
092600040119     C                   EVAL      PiStr=SkSplitCSV(i)
092700040119     C                   EXSR      CHKNUM
092800040119     C                   IF        PiInt=*on
092900040119     C                   Z-ADD     PiVal         VABRMN
093000040119     C                   Z-ADD     PiVal         VABNSP
093100040119     C                   Z-ADD     PiVal         VATNSP
093200040119     C                   MOVE      VABRMN        wAlfa4
093300040119     C                   EVAL      VABRMA = %trim(VABRMA)+wAlfa4
093400040119     C                   EVAL      VATNOT = %trim(VATNOT)+wAlfa4
093500040119     C                   ELSE
093600040119     C                   SETON                                        32
093700040119     C                   EVAL      VABRMN = 1
093800040119     C                   EVAL      vinmsg = %trimr(vinmsg)
093900040119     C                             + ' ' + 'VABRMN VABNSP VATNSP VATNOT'
094000040119     C                   ENDIF
094100040119     C                   ENDIF
094200040119     C*** 3° parte VATNOT
094300040119     C                   IF        %trim(SkSplitFLD(i)) = 'ANNO'
094400040119     C                   EVAL      VATNOT = %trim(VATNOT)+%trim(SkSplitCSV(i))
094500040119     C                   ENDIF
094600040119     C*** CBO
094700040119     C                   IF        %trim(SkSplitFLD(i)) = 'TIPOPORTO'
094800040119     C                   IF        %trim(SkSplitCSV(i)) = '1'
094900040119     C                   EVAL      VABCBO = '2'
095000040119     C                   ENDIF
095100040119     C                   IF        %trim(SkSplitCSV(i)) = '2'
095200040119     C                   EVAL      VABCBO = '1'
095300040119     C                   ENDIF
095400040119     C                   ENDIF
095500030721     C*
095600030721     C* Reperisco quindi i campi numerici...
095700030721     C*** CAD
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
095900030721     C                   EVAL      PiStr=SkSplitCSV(i)
096000030721     C                   EXSR      CHKNUM
096100030721     C                   IF        PiInt=*on
096200030721     C                   Z-ADD     PiVal         Num5_0
096300030721     C                   MOVEL(p)  Num5_0        VABCAD
096400030721     C                   ELSE
096500030721     C                   SETON                                        32
096600030721     C                   EVAL      VABCAD = *zeros
096700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096800030721     C                             + ' ' + 'VABCAD'
096900030721     C                   ENDIF
097000030721     C                   ENDIF
097100030721     C*** NCL
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
097300030721     C                   EVAL      PiStr=SkSplitCSV(i)
097400030721     C                   EXSR      CHKNUM
097500030721     C                   IF        PiInt=*on
097600030721     C                   Z-ADD     PiVal         VABNCL
097700030721     C                   ELSE
097800030721     C                   SETON                                        32
097900030721     C                   EVAL      VABNCL = *zeros
098000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098100040119     C                             + ' ' + 'VABNCL VATNOT'
098200030721     C                   ENDIF
098300030721     C                   ENDIF
098400030721     C*** PKB
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
098600030721     C                   EVAL      PiStr=SkSplitCSV(i)
098700030721     C                   EXSR      CHKNUM
098800030721     C                   IF        PiNum=*on
098900030721     C                   Z-ADD     PiVal         VABPKB
099000030721     C                   ELSE
099100030721     C                   SETON                                        32
099200030721     C                   EVAL      VABPKB = *zeros
099300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099400030721     C                             + ' ' + 'VABPKB'
099500030721     C                   ENDIF
099600030721     C                   ENDIF
099700030721     C*** CAS
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
099900030721     C                   EVAL      PiStr=SkSplitCSV(i)
100000030721     C                   EXSR      CHKNUM
100100030721     C                   IF        PiNum=*on
100200030721     C                   Z-ADD     PiVal         VABCAS
100300030721     C                   ELSE
100400030721     C                   SETON                                        32
100500030721     C                   EVAL      VABCAS = *zeros
100600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100700030721     C                             + ' ' + 'VABCAS'
100800030721     C                   ENDIF
100900030721     C                   ENDIF
101000030721     C*** CCM
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
101200030721     C                   EVAL      PiStr=SkSplitCSV(i)
101300030721     C                   EXSR      CHKNUM
101400030721     C                   IF        PiInt=*on      AND
101500030721     C                             PiVal<=9999999 AND
101600030721     C                             PiVal>*zeros
101700030721     C                   Z-ADD     PiVal         VABCCM
101800040119     C                   Z-ADD     PiVal         VATCCM
101900030721     C                   ELSE
102000030721     C                   SETON                                        32
102100030721     C                   EVAL      VABCCM = *zeros
102200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102300040119     C                             + ' ' + 'VABCCM VATCCM'
102400030721     C                   ENDIF
102500030721     C                   ENDIF
102600030721     C*** LNP
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
102800030721     C                   EVAL      PiStr=SkSplitCSV(i)
102900030721     C                   EXSR      CHKNUM
103000030721     C                   IF        PiInt=*on    AND
103100030721     C                             PiVal<=999   AND
103200030721     C                             PiVal>*zeros
103300030721     C                   Z-ADD     PiVal         VABLNP
103400040119     C                   Z-ADD     PiVal         VATLNP
103500030721     C                   ELSE
103600030721     C                   SETON                                        32
103700030721     C                   EVAL      VABLNP = *zeros
103800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103900040119     C                             + ' ' + 'VABLNP VATLNP'
104000030721     C                   ENDIF
104100030721     C                   ENDIF
104200030721     C*** NRS
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
104400030721     C                   EVAL      PiStr=SkSplitCSV(i)
104500030721     C                   EXSR      CHKNUM
104600030721     C                   IF        PiInt=*on AND
104700030721     C                             PiVal<=99
104800030721     C                   Z-ADD     PiVal         VABNRS
104900030721     C                   ELSE
105000030721     C                   SETON                                        32
105100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105200030721     C                             + ' ' + 'VABNRS'
105300030721     C                   ENDIF
105400030721     C                   ENDIF
105500030721     C*** LNA
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
105700030721     C                   EVAL      PiStr=SkSplitCSV(i)
105800030721     C                   EXSR      CHKNUM
105900030721     C                   IF        PiInt=*on    AND
106000030721     C                             PiVal<=999   AND
106100030721     C                             PiVal>*zeros
106200030721     C                   Z-ADD     PiVal         VABLNA
106300030721     C                   ELSE
106400030721     C                   SETON                                        32
106500030721     C                   EVAL      VABLNA = *zeros
106600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106700030721     C                             + ' ' + 'VABLNA'
106800030721     C                   ENDIF
106900030721     C                   ENDIF
107000030721     C*** CTR
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
107200030721     C                   EVAL      PiStr=SkSplitCSV(i)
107300030721     C                   EXSR      CHKNUM
107400030721     C                   IF        PiInt=*on  AND
107500030721     C                             PiVal<=999
107600030721     C                   Z-ADD     PiVal         VABCTR
107700030721     C                   ELSE
107800030721     C                   SETON                                        32
107900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108000030721     C                             + ' ' + 'VABCTR'
108100030721     C                   ENDIF
108200030721     C                   ENDIF
108300030721     C*** IAS
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
108500030721     C                   EVAL      PiStr=SkSplitCSV(i)
108600030721     C                   EXSR      CHKNUM
108700030721     C                   IF        PiNum=*on
108800030721     C                   Z-ADD     PiVal         VABIAS
108900030721     C                   ELSE
109000030721     C                   SETON                                        32
109100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109200030721     C                             + ' ' + 'VABIAS'
109300030721     C                   ENDIF
109400030721     C                   ENDIF
109500030721     C*** VLB
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
109700030721     C                   EVAL      PiStr=SkSplitCSV(i)
109800030721     C                   EXSR      CHKNUM
109900030721     C                   IF        PiNum=*on
110000030721     C                   Z-ADD     PiVal         VABVLB
110100030721     C                   ELSE
110200030721     C                   SETON                                        32
110300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110400030721     C                             + ' ' + 'VABVLB'
110500030721     C                   ENDIF
110600030721     C                   ENDIF
110700030721     C*** QFT
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
110900030721     C                   EVAL      PiStr=SkSplitCSV(i)
111000030721     C                   EXSR      CHKNUM
111100030721     C                   IF        PiNum=*on
111200030721     C                   Z-ADD     PiVal         VABQFT
111300030721     C                   ELSE
111400030721     C                   SETON                                        32
111500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111600030721     C                             + ' ' + 'VABQFT'
111700030721     C                   ENDIF
111800030721     C                   ENDIF
111900030721     C*** NCD
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
112100030721     C                   EVAL      PiStr=SkSplitCSV(i)
112200030721     C                   EXSR      CHKNUM
112300030721     C                   IF        PiInt=*on      AND
112400030721     C                             PiVal<=9999999
112500030721     C                   Z-ADD     PiVal         VABNCD
112600030721     C                   ELSE
112700030721     C                   SETON                                        32
112800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112900030721     C                             + ' ' + 'VABNCD'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200030721     C*** NCA
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on      AND
113700030721     C                             PiVal<=9999999
113800030721     C                   Z-ADD     PiVal         VABNCA
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200030721     C                             + ' ' + 'VABNCA'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500030721     C*** ZNC
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiInt=*on AND
115000030721     C                             PiVal<=99
115100030721     C                   Z-ADD     PiVal         VABZNC
115200030721     C                   ELSE
115300030721     C                   SETON                                        32
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500030721     C                             + ' ' + 'VABZNC'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** DCR
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiInt=*on       AND
116300030721     C                             PiVal<=99999999
116400030721     C                   Z-ADD     PiVal         VABDCR
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800030721     C                             + ' ' + 'VABDCR'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** HCR
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on   AND
117600030721     C                             PiVal<=9999
117700030721     C                   Z-ADD     PiVal         VABHCR
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABHCR'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** VMD
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiNum=*on
118900030721     C                   Z-ADD     PiVal         VABVMD
119000030721     C                   ELSE
119100030721     C                   SETON                                        32
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300030721     C                             + ' ' + 'VABVMD'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** ANT
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiInt=*on        AND
120100030721     C                             PiVal<=999999999
120200030721     C                   Z-ADD     PiVal         VABANT
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600030721     C                             + ' ' + 'VABANT'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** CMO
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiInt=*on
121400030721     C                   Z-ADD     PiVal         Num5_0
121500030721     C                   MOVEL(p)  Num5_0        VABCMO
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      VABCMO = *zeros
121900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122000030721     C                             + ' ' + 'VABCMO'
122100030721     C                   ENDIF
122200030721     C                   ENDIF
122300030715     C*
122400030715     C                   ENDIF
122500030715     C                   EVAL      i = i + 1
122600030715     C                   ENDDO
122700040113     C*
122800040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
122900040113     C                   IF        VABCAS > *zeros
123000040119     C                   EVAL      VABVCA = 'EUR'
123100040119     C                   IF        VABCBO = '1'
123200040113     C                   EVAL      VABCBO = '4'
123300040119     C                   ENDIF
123400040119     C                   IF        VABCBO = '2'
123500040119     C                   EVAL      VABCBO = '6'
123600040119     C                   ENDIF
123700040113     C                   ENDIF
123800020204     C*
123900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
124000020204     C                   EXSR      CHKIMPDIV
124100030715     C                   ENDIF
124200020204     C*
124300010607     C                   ENDSR
124400010607     C*----------------------------------------------------*
124500010601
124600020204
124700020204     C*----------------------------------------------------*
124800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
124900020204     C*----------------------------------------------------*
125000020204     C     CHKIMPDIV     BEGSR
125100020204     C*
125200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
125300020204     C                   Z-ADD     *zeros        wrkDec            9 9
125400020204     C*
125500020204     C* Come prima cosa effettuo considerazioni sulla divisa
125600020204     C                   IF        vabIAS > *zeros
125700020204     C                   IF        vabVAS <> 'EUR'
125800020204     C                   EVAL      vabVAS =  'ITL'
125900020204     C                   ENDIF
126000020204     C                   ENDIF
126100020204     C*
126200020204     C                   IF        vabCAS > *zeros
126300020204     C                   IF        vabVCA <> 'EUR'
126400020204     C                   EVAL      vabVCA =  'ITL'
126500020204     C                   ENDIF
126600020204     C                   ENDIF
126700020204     C*
126800020204     C                   IF        vabVMD > *zeros
126900020204     C                   IF        vabVAD <> 'EUR'
127000020204     C                   EVAL      vabVAD =  'ITL'
127100020204     C                   ENDIF
127200020204     C                   ENDIF
127300020204     C*
127400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
127500020204     C                   Z-ADD     vabIAS        wrkDec
127600020204     C                   IF        wrkDec > *zeros
127700020204     C                   IF        vabVAS = 'ITL'
127800020204     C                   EVAL      vabIAS = *zeros
127900020204     C                   ENDIF
128000020204     C                   ENDIF
128100020204     C*
128200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
128300020204     C                   Z-ADD     vabCAS        wrkDec
128400020204     C                   IF        wrkDec > *zeros
128500020204     C                   IF        vabVCA = 'ITL'
128600020204     C                   EVAL      vabCAS = *zeros
128700020204     C                   ENDIF
128800020204     C                   ENDIF
128900020204     C*
129000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
129100020204     C                   Z-ADD     vabVMD        wrkDec
129200020204     C                   IF        wrkDec > *zeros
129300020204     C                   IF        vabVAD = 'ITL'
129400020204     C                   EVAL      vabVMD = *zeros
129500020204     C                   ENDIF
129600020204     C                   ENDIF
129700020204     C*
129800020204     C                   ENDSR
129900020204     C***
130000020204
130100010330
130200010330
130300010330     C*----------------------------------------------------*
130400010330     C*  CONTROLLO NUMERICITA' CAMPI
130500010330     C*----------------------------------------------------*
130600010330     C     CHKNUM        BEGSR
130700010330     C*
130800010606     C                   IF        PiDecChr = *blanks
130900030715     C                   EVAL      PiDecChr = CharNUM
131000010606     C                   ENDIF
131100010606     C*
131200010606     C                   CALL(e)   'ISNUMERIC'
131300010330     C                   PARM                    PiStr            30
131400010606     C                   PARM                    PiDecChr          1
131500010330     C                   PARM      *ZEROS        PiVal            30 9
131600010330     C                   PARM      '0'           PiInt             1
131700010330     C                   PARM      '0'           PiNum             1
131800010330     C                   IF        %error
131900010606     C                   EVAL      PiNum=*off
132000010330     C                   ENDIF
132100010330     C*
132200010330     C                   ENDSR
132300010330     C***
132400010330
132500010601
132600010601
132700010601
132800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
132900010601     C     repfil        BEGSR
133000010601     C*
133100010601     C                   if        invfil = *zeros and
133200010601     C                             depfil > *zeros and
133300010629     C                             (vinflg = *blanks or
133400010629     C                              vinflg = *zeros)
133500010601     C
133600010601     C                   eval      invfil = depfil
133700010601     C                   endif
133800010601     C*
133900010601     C                   if        depfil <> invfil and
134000010601     C                             invfil > *zeros
134100010601     C                   eval      flgMulti = '1'
134200010601     C                   if        vinflg = *blanks
134300010601     C                   add       1             cntNonEl
134400010601     C                   endif
134500010601     C                   endif
134600010601     C*
134700010601     C                   if        vinflg = '2'
134800010601     C                   eval      flgStato = '2'
134900010601     C                   endif
135000010601     C*
135100010601     C                   ENDSR
135200010601     C***
135300010601
135400010601
135500010601
135600010330
135700040119      /TITLE Invio dei dati al punto operativo.
135800040119     C     invio         BEGSR
135900040119     C*
136000040119     C* 1° invio FIVAT
136100040119     C                   reset                   dscmz
136200040119     C                   move      vlrpoi        cmzdst
136300040119     C                   eval      cmzfld = 'FIVATWWR'
136400040119     C                   eval      cmzmbd = vlrhdl
136500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
136600040119     C***                if        prmfir = *blanks
136700040119     C                   eval      cmzfla = 'FIVAT00F'
136800040119     C                   eval      cmzmba = 'FIVAT00F'
136900040119     C***                else
137000040119     C***                eval      cmzfla = prmfir
137100040119     C***                eval      cmzmba = prmfir
137200040119     C***                endif
137300040119     C                   eval      cmznrr = *zeros
137400040119     C                   move      §ctrokvt      cmznrr
137500040119     C                   eval      cmzlba = vlrfl1
137600040119     C                   call(e)   'TIS711C'
137700040119     C                   parm                    dscmz
137800040119     C                   parm      *blanks       esito
137900040119     C                   if        %error
138000040119     C                             or cmzerr = '1'
138100040119     C                             or esito  = '1'
138200040119     C                   eval      wrkesito = '3'
138300040119     C                   else
138400040119     C*
138500040119     C* 2° invio FIVAB
138600040119     C                   reset                   dscmz
138700040119     C                   move      vlrpoi        cmzdst
138800040119     C                   eval      cmzfld = vlrfou
138900040119     C                   eval      cmzmbd = vlrhdl
139000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
139100040119     C***                if        prmfir = *blanks
139200040119     C                   eval      cmzfla = 'FIVAB00F'
139300040119     C                   eval      cmzmba = 'FIVAB00F'
139400040119     C***                else
139500040119     C***                eval      cmzfla = prmfir
139600040119     C***                eval      cmzmba = prmfir
139700040119     C***                endif
139800040119     C                   eval      cmznrr = *zeros
139900040119     C                   move      §ctrokvb      cmznrr
140000040119     C                   eval      cmzlba = vlrfl1
140100040119     C                   call(e)   'TIS711C'
140200040119     C                   parm                    dscmz
140300040119     C                   parm      *blanks       esito
140400040119     C                   if        %error
140500040119     C                             or cmzerr = '1'
140600040119     C                             or esito  = '1'
140700040119     C                   eval      wrkesito = '3'
140800040119     C                   endif
140900040119     C                   endif
141000040119     C*
141100040119     C                   ENDSR
141200040119     C***
141300010601
141400010601
141500010601
141600010601
141700010601      /TITLE Invio dei dati al punto operativo.
141800010601     C     opeini        BEGSR
141900010601     C*
142000010601     C* Inizializzo flag e contatori operativi
142100010601     C                   movel     '0'           flgGiro           1
142200010601     C                   movel     '0'           flgMulti          1
142300010601     C                   movel     '1'           flgStato          1
142400010615     C                   movel     '0'           flgOk             1
142500010601     C                   z-add     *zeros        cntNonEl         10 0
142600010601     C                   z-add     *zeros        depfil            3 0
142700010601     C                   z-add     *zeros        invfil            3 0
142800010601     C*
142900010601     C                   ENDSR
143000010601     C***
143100070411
143200070411     C     *pssr         BEGSR
143300070411     C*
143400070411     C                   if        %open(tivin00r)
143500070411     C                   close     tivin00r
143600070411     C                   endif
143700070411     C                   if        %open(fivabwwr)
143800070411     C                   close     fivabwwr
143900070411     C                   endif
144000070411     C                   if        %open(fivatwwr)
144100070411     C                   close     fivatwwr
144200070411     C                   endif
144300070411     C*
144400070411     C* Effettuo la chiamata al CLLE preposto
144500070411     C                   call(e)   'TITVVTC'
144600070411     C                   parm                    parccm
144700070411     C                   parm                    parmbr
144800070411     C                   parm      '2'           paropz
144900070411     C*
145000070411     C                   eval      wrkesito = '2'
145100070411     C*
145200070411     C                   seton                                        LR
145300070411     C*
145400070411     C                   ENDSR     '*CANCL'
145500070411     C***
145600070411
145700050421
145800010330
145900010330
146000000613     C     *inzsr        BEGSR
146100990910     C*
146200990910     C     *entry        plist
146300990920     C                   parm                    tivlrds
146400990921     C                   parm      wrkesito      esito
146500000724     C                   parm                    prmlit
146600000710     C                   parm                    prmfir
146700010330     C*
146800010330     C* CALCOLA LA DATA CORRENTE
146900010330     C                   time                    wn14             14 0
147000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
147100010330     C                   z-add     wn8           g08dat
147200010330     C                   z-add     *zeros        g08inv
147300010330     C                   movel     '0'           g08err
147400010330     C                   call      'XSRDA8'
147500010330     C                   parm                    wlbda8
147600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
147700000613     C*
147800000613     C                   ENDSR
147900000613     C***
148000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
148100040119bcalfa;vabrmn;anno;dataora;vabrsd;vabind;vabcad;vablod;
148200040119vabprd;naturamerce;vabncl;vabpkb;tipoporto;vabcas
148300040113
