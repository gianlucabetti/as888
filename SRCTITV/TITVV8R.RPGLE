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
002500040709     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
002600040119     D parccm          s              8    INZ(*blanks)
002700040119     D parmbr          s             10    INZ(*blanks)
002800040119     D paropz          s              1    INZ(*blanks)
002900040119     D chkcall         s              1    INZ(*blanks)
003000040719     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
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
005800031201     D*------------------
005900031201     D* Costanti
006000031201     D*------------------
006100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006300010330
006400010330
006500010330
006600000913     C                   reset                   rrnum
006700990921     C                   reset                   esito
006800990921     C                   reset                   wrkesito
006900010601     C*
007000010601     C                   exsr      opeini
007100010605     C                   exsr      rwvab
007200010601     C*
007300010601     C                   seton                                        lr
007400010601
007500010601
007600010601
007700010601     C*--------------------------------------------------------
007800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007900010601     C*--------------------------------------------------------
008000010601     C     PREELA        BEGSR
008100010601     C*
008200010601     C* SE OCCORRE SPEDIRE IN FILIALE
008300010601     C                   if        invfil <> *zeros and
008400010601     C                             flgGiro = '0'
008500010601     C*
008600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008700010601     C                   eval      flgGiro = '1'
008800010601     C*
008900010601     C                   endif
009000010601     C*
009100010601     C                   ENDSR
009200010601     C***
009300010601
009400010601
009500010601
009600010601
009700010601     C*--------------------------------------------------------
009800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009900010601     C*--------------------------------------------------------
010000010601     C     ENDELA        BEGSR
010100040119     C*
010200040119     C* Effettuo la chiamata al CLLE preposto
010300040119     C                   call(e)   'TITVVTC'
010400040119     C                   parm                    parccm
010500040119     C                   parm                    parmbr
010600040119     C                   parm      '2'           paropz
010700050201     C*
010800050201     C* Effettuo lancio TISI95 solo x chiusura
010900050201     C                   CLEAR                   TISI95DS
011000050201     C                   EVAL      I95TLA = 'C'
011100050201     C                   CALL      'TISI95R'
011200050201     C                   PARM                    TISI95DS
011300000616     C*
011400010601     C                   ENDSR
011500010601     C***
011600000613
011700010601
011800010601
011900010330     C*--------------------------------------------------------
012000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
012100010330     C*--------------------------------------------------------
012200010605     C     RWVAB         BEGSR
012300010330     C*
012400010330     C                   if        not %open(tivin00r)
012500010330     C                   open      tivin00r
012600010330     C                   endif
012700021025     C                   if        not %open(fivabwwr)
012800021025     C                   open      fivabwwr
012900010330     C                   endif
013000040119     C*
013100040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013200040119     C                   exsr      prevat
013300040119     C*
013400040119     C                   if        chkcall = '0'
013500040119     C*
013600040119     C                   if        not %open(fivatwwr)
013700040119     C                   open      fivatwwr
013800040119     C                   endif
013900010330     C*
014000040119     C                   clear                   §CTROKVB          7 0
014100040119     C                   clear                   §CTROKVT          7 0
014200040119     C                   clear                   §CTRMO            7 0
014300040119     C                   clear                   §CTRNO            7 0
014400040719     C*
014500040719     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
014600040719     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
014700040719     C                   exsr      inzvar
014800040719     C                   exsr      impvab
014900040706     C*
015000040706     C  N70              do        *HIVAL
015100010330     C*
015200040706     C                   read      tivin00r                               70
015300010618     C*
015400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015500010618     C                   if        vinflg = '1'
015600010618     C                   eval      flgOk = '1'
015700010618     C                   endif
015800010618     C*
015900040316     C                   if        (vindta > *blanks AND
016000040316     C                              %trim(vindta) <> *all';') OR
016100040316     C                             wGiro = *zeros
016200010330     C                   add       1             rrnum
016300010330     C*
016400010601     C                   if        *in70 = *off and
016500010330     C                             (vinflg = *blanks
016600010330     C                              or vinflg = '0'
016700010330     C                              or vinflg = '2')
016800010330     C*
016900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017000010711     C                   if        vinflg = *blanks or vinflg = '0'
017100010711     C                   clear                   vinmsg
017200010711     C                   endif
017300010601     C*
017400010330     C                   exsr      inzvar
017500010605     C                   exsr      impvab
017600010601     C*
017700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017800010605     C                   eval      depfil = VABLNP
017900010601     C                   exsr      repfil
018000010601     C                   if        depfil = invfil
018100021025     C                   if        vlrpoi = 999
018200030822     C                   move(P)   invfil        VABFGS
018300040119     C                   move(P)   invfil        VATFGS
018400021025     C                   else
018500030822     C                   move(P)   vlrpoi        VABFGS
018600040119     C                   move(P)   vlrpoi        VATFGS
018700021025     C                   endif
018800010601     C*
018900010601     C                   exsr      PREELA
019000010601     C*
019100010604     C* Ebbene...
019200010604     C*
019300040119     C  N31              add       1             §CTROKVB
019400040119     C   32              add       1             §CTRMO
019500040119     C   31              add       1             §CTRNO
019600030822     C                   if        wGiro = 2
019700040119     C                   if        depspe =  *zeros OR
019800040119     C                             depspe <> vabNSP
019900040119     C  N31              exsr      wrivab
020000040119     C  N31              eval      depspe = vabNSP
020100040119     C                   endif
020200040119     C                   endif
020300010604     C*
020400010604     C                   if        *in31 = *off and
020500010604     C                             *in32 = *off
020600010604     C                   eval      vinflg = '1'
020700010604     C                   else
020800010604     C                   eval      vinflg = '2'
020900010604     C                   endif
021000010604     C                   endif
021100010601     C*
021200010604     C                   endif
021300010604     C*
021400010330     C                   else
021500010330     C                   eval      vinflg = '1'
021600010330     C                   endif
021700010601     C*
021800010601     C  N70              update    tivin000
021900010330     C*
022000030822     C  N70              enddo
022100040119     C*
022200040119     C                   endif                                                  (endif - chkcall)
022300010601     C*
022400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
022500010601     C                   if        cntNonEl = *zeros or
022600010601     C                             flgMulti = '0'
022700010330     C* Se non ci sono record con errori ...
022800010601     C                   if        §ctrno = 0 and
022900010604     C                             §ctrmo = 0 and
023000010601     C                             flgStato <> '2'
023100010330     C* ... restituisco esito OK.
023200010330     C                   eval      wrkesito = '0'
023300010330     C                   else
023400040119     C                   if        §ctrokvb > 0
023500010330     C                   eval      wrkesito = '1'
023600010330     C                   else
023700010615     C                   if        flgOk = '0'
023800010615     C                   eval      wrkesito = '2'
023900010615     C                   else
024000010615     C                   eval      wrkesito = '6'
024100010615     C                   endif
024200010330     C                   endif
024300010330     C                   endif
024400010601     C                   else
024500010601     C                   eval      wrkesito = '9'
024600010601     C                   endif
024700010330     C*
024800010330     C                   if        %open(tivin00r)
024900010330     C                   close     tivin00r
025000010330     C                   endif
025100021025     C                   if        %open(fivabwwr)
025200021025     C                   close     fivabwwr
025300010330     C                   endif
025400040512     C                   if        %open(fivatwwr)
025500040512     C                   close     fivatwwr
025600040512     C                   endif
025700010601     C*
025800010601     C                   if        vlrpoi <> 999
025900010601     C                   eval      invfil = vlrpoi
026000010601     C                   endif
026100010330     C*
026200040119     C                   if        §ctrokvb > 0
026300010601     C                             and invfil > *zeros
026400010330     C                   exsr      invio
026500010330     C                   endif
026600010601     C*
026700010618     C                   if        flgGiro = '1'
026800010601     C                   exsr      endela
026900010618     C                   endif
027000010330     C*
027100010330     C                   ENDSR
027200010330     C***
027300040119
027400040119
027500040119
027600040119     C*----------------------------------------------------*
027700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
027800040119     C*----------------------------------------------------*
027900040119     C     WRIVAB        BEGSR
028000040119     C*
028100040119     C                   write     fivab000                                     => scarico il VAB
028200040119     C*
028300040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
028400040119     C                   exsr      wrivat
028500040119     C*
028600040119     C                   ENDSR
028700040119
028800040119
028900040119
029000040119     C*----------------------------------------------------*
029100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
029200040119     C*----------------------------------------------------*
029300040706     C     WRIVAT        BEGSR
029400040706     C*
029500040719     C* Attribuisco la stessa chiave della testata bolle
029600040719     C                   EVAL      VATAAS = VABAAS
029700040719     C                   EVAL      VATLNP = VABLNP
029800040719     C                   EVAL      VATNSP = VABNSP
029900040719     C*
030000061211     C* Imposto il tipo record sul file VAT a 'E' x indicare il "chi sono"
030100061211     C                   EVAL      VATTRC = 'E'
030200040719     C*
030300040719     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
030400040719     C                   IF        VATNOT <> *blanks
030500040706     C                   WRITE     FIVAT000
030600040706     C                   ADD       1             §CTROKVT
030700040719     C                   ENDIF
030800040119     C*
030900040119     C                   ENDSR
031000040119
031100040119
031200040119
031300040119     C*----------------------------------------------------*
031400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
031500040119     C*----------------------------------------------------*
031600040119     C     PREVAT        BEGSR
031700040119     C*
031800040119     C* Compongo il nome del membro da dare al FIVATWWR
031900040119     C                   eval      parmbr = vlrhdl
032000040119     C                   movel     'M'           parmbr
032100061211     C                   eval      parccm = vlrksc
032200040119     C                   eval      paropz = '1'
032300040119     C* Effettuo la chiamata al CLLE preposto
032400040119     C                   call(e)   'TITVVTC'
032500040119     C                   parm                    parccm
032600040119     C                   parm                    parmbr
032700040119     C                   parm                    paropz
032800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
032900040119     C                   if        %error
033000040119     C                   movel     '1'           chkcall
033100040119     C                   else
033200040119     C                   movel     '0'           chkcall
033300040119     C                   endif
033400040119     C*
033500040119     C                   ENDSR
033600010601
033700010601
033800010601
033900010330     C*----------------------------------------------------*
034000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
034100010330     C*----------------------------------------------------*
034200040119     C     INZVAR        BEGSR
034300010330     C*
034400030822     C* Inizializzo variabili di wrk
034500020204     C                   Z-ADD     *zeros        Num5_0            5 0
034600030822     C*
034700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
034800030822     C                   CLEAR                   FIVAB000
034900040119     C                   CLEAR                   FIVAT000
035000030822     C                   CLEAR                   SkSplitCSV
035100030822     C                   EVAL      i = 1
035200030822     C                   EVAL      posDa = *zeros
035300030822     C                   EVAL      posA  = *zeros
035400030822     C*
035500030822     C* Reimposto i valori di default
035600030822     C                   EXSR      DEFCAM
035700030822     C*
035800010330     C                   ENDSR
035900010330     C*----------------------------------------------------*
036000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
036100010330     C*----------------------------------------------------*
036200010330     C     DEFCAM        BEGSR
036300040119     C*
036400040119     C* Imposto i valori d default a programma dei campi fondamentali
036500061211     C                   EVAL      VABCCM = 0012943
036600061211     C                   EVAL      VATCCM = 0012943
036700040719     C                   EVAL      VABLNP = 001
036800040719     C                   EVAL      VATLNP = 001
036900061211     C                   EVAL      VABCTR = 300
037000080124     C                   EVAL      VABCBO = '1'
037100061211     C                   EVAL      VABCTM = '7Q'
037200061211     C                   EVAL      VABVAS = 'EUR'
037300061211     C                   EVAL      VABVCA = 'EUR'
037400030822     C*
037500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
037600030715     C* e delimitatore testo.
037700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
037800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
037900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
038000030822     C*
038100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
038200030715     C                   IF        %subst(vlrppt:1:1) = 'S'
038300030715     C                   SETON                                        50
038400030715     C                   ELSE
038500030715     C                   SETOFF                                       50
038600030715     C                   ENDIF
038700030822     C*
038800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
038900030822     C                   EVAL      posDaDft = 1
039000030822     C                   EVAL      posADft  = 0
039100030822     C                   EVAL      wGiroDft = 0
039200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
039300030822     C                             posDaDft > 0
039400030822     C*
039500030822     C* Gestisco il 1° giro
039600030822     C                   IF        wGiroDft = 0
039700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
039800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
039900030822     C* Incremento il contatore dei "giri"
040000030822     C                   EVAL      wGiroDft = 1
040100030822     C                   ELSE
040200030822     C                   EVAL      posDaDft = posADft
040300030822     C                   ENDIF
040400030822     C* Eseguo lo scan x trovare la fine del campo corrente
040500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
040600030822     C*
040700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
040800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
040900030822     C                   IF        posDaDft > 0 AND
041000030822     C                             posADft  > 0
041100030822     C* NCL
041200030822     C                   IF        %subst(
041300030822     C                             %subst(vlrppt:posDaDft+1:
041400030822     C                             posADft-posDaDft-1):1:3)
041500030822     C                             = 'NCL'
041600030822     C                   EVAL      PiStr=%trim(%subst(
041700030822     C                             %subst(vlrppt:posDaDft+1:
041800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041900030822     C                   EXSR      CHKNUM
042000030822     C                   IF        PiInt=*on
042100030822     C                   Z-ADD     PiVal         VABNCL
042200030822     C                   ENDIF
042300030822     C                   ENDIF
042400030822     C* CCM
042500030822     C                   IF        %subst(
042600030822     C                             %subst(vlrppt:posDaDft+1:
042700030822     C                             posADft-posDaDft-1):1:3)
042800030822     C                             = 'CCM'
042900030822     C                   EVAL      PiStr=%trim(%subst(
043000030822     C                             %subst(vlrppt:posDaDft+1:
043100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043200030822     C                   EXSR      CHKNUM
043300030822     C                   IF        PiInt=*on
043400030822     C                   Z-ADD     PiVal         VABCCM
043500040119     C                   Z-ADD     PiVal         VATCCM
043600030822     C                   ENDIF
043700030822     C                   ENDIF
043800030822     C* LNP
043900030822     C                   IF        %subst(
044000030822     C                             %subst(vlrppt:posDaDft+1:
044100030822     C                             posADft-posDaDft-1):1:3)
044200030822     C                             = 'LNP'
044300030822     C                   EVAL      PiStr=%trim(%subst(
044400030822     C                             %subst(vlrppt:posDaDft+1:
044500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044600030822     C                   EXSR      CHKNUM
044700030822     C                   IF        PiInt=*on
044800030822     C                   Z-ADD     PiVal         VABLNP
044900040119     C                   Z-ADD     PiVal         VATLNP
045000030822     C                   ENDIF
045100030822     C                   ENDIF
045200030822     C* NRS
045300030822     C                   IF        %subst(
045400030822     C                             %subst(vlrppt:posDaDft+1:
045500030822     C                             posADft-posDaDft-1):1:3)
045600030822     C                             = 'NRS'
045700030822     C                   EVAL      PiStr=%trim(%subst(
045800030822     C                             %subst(vlrppt:posDaDft+1:
045900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046000030822     C                   EXSR      CHKNUM
046100030822     C                   IF        PiInt=*on
046200030822     C                   Z-ADD     PiVal         VABNRS
046300030822     C                   ENDIF
046400030822     C                   ENDIF
046500030822     C* CTR
046600030822     C                   IF        %subst(
046700030822     C                             %subst(vlrppt:posDaDft+1:
046800030822     C                             posADft-posDaDft-1):1:3)
046900030822     C                             = 'CTR'
047000030822     C                   EVAL      PiStr=%trim(%subst(
047100030822     C                             %subst(vlrppt:posDaDft+1:
047200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047300030822     C                   EXSR      CHKNUM
047400030822     C                   IF        PiInt=*on
047500030822     C                   Z-ADD     PiVal         VABCTR
047600030822     C                   ENDIF
047700030822     C                   ENDIF
047800030822     C* PKB
047900030822     C                   IF        %subst(
048000030822     C                             %subst(vlrppt:posDaDft+1:
048100030822     C                             posADft-posDaDft-1):1:3)
048200030822     C                             = 'PKB'
048300030822     C                   EVAL      PiStr=%trim(%subst(
048400030822     C                             %subst(vlrppt:posDaDft+1:
048500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048600030822     C                   EXSR      CHKNUM
048700030822     C                   IF        PiNum=*on
048800030822     C                   Z-ADD     PiVal         VABPKB
048900030822     C                   ENDIF
049000030822     C                   ENDIF
049100030822     C* VLB
049200030822     C                   IF        %subst(
049300030822     C                             %subst(vlrppt:posDaDft+1:
049400030822     C                             posADft-posDaDft-1):1:3)
049500030822     C                             = 'VLB'
049600030822     C                   EVAL      PiStr=%trim(%subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049900030822     C                   EXSR      CHKNUM
050000030822     C                   IF        PiNum=*on
050100030822     C                   Z-ADD     PiVal         VABVLB
050200030822     C                   ENDIF
050300030822     C                   ENDIF
050400030822     C* QFT
050500030822     C                   IF        %subst(
050600030822     C                             %subst(vlrppt:posDaDft+1:
050700030822     C                             posADft-posDaDft-1):1:3)
050800030822     C                             = 'QFT'
050900030822     C                   EVAL      PiStr=%trim(%subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051200030822     C                   EXSR      CHKNUM
051300030822     C                   IF        PiNum=*on
051400030822     C                   Z-ADD     PiVal         VABQFT
051500030822     C                   ENDIF
051600030822     C                   ENDIF
051700030822     C* CBO
051800030822     C                   IF        %subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):1:3)
052100030822     C                             = 'CBO'
052200030822     C                   EVAL      VABCBO=%trim(%subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052500030822     C                   ENDIF
052600030822     C* TSP
052700030822     C                   IF        %subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):1:3)
053000030822     C                             = 'TSP'
053100030822     C                   EVAL      VABTSP=%trim(%subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053400030822     C                   ENDIF
053500030822     C* VAS
053600030822     C                   IF        %subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):1:3)
053900030822     C                             = 'VAS'
054000030822     C                   EVAL      VABVAS=%trim(%subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300030822     C                   ENDIF
054400030822     C* VCA
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800030822     C                             = 'VCA'
054900030822     C                   EVAL      VABVCA=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   ENDIF
055300030822     C* GCA
055400030822     C                   IF        %subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):1:3)
055700030822     C                             = 'GCA'
055800030822     C                   EVAL      VABGCA=%trim(%subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056100030822     C                   ENDIF
056200030822     C* CTM
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'CTM'
056700030822     C                   EVAL      VABCTM=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   ENDIF
057100030822     C* FFD
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'FFD'
057600030822     C                   EVAL      VABFFD=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   ENDIF
058000030822     C* VAD
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'VAD'
058500030822     C                   EVAL      VABVAD=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   ENDIF
058900030822     C* GMA
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'GMA'
059400030822     C                   EVAL      VABGMA=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   ENDIF
059800030822     C* GGA
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'GGA'
060300030822     C                   EVAL      VABGGA=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   ENDIF
060700030822     C* GVA
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'GVA'
061200030822     C                   EVAL      VABGVA=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   ENDIF
061600030822     C* TC1
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'TC1'
062100030822     C                   EVAL      VABTC1=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   ENDIF
062500030822     C* TC2
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'TC2'
063000030822     C                   EVAL      VABTC2=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   ENDIF
063400030822     C* ...
063500030822     C                   ENDIF
063600030822     C                   ENDDO
063700020204     C*
063800010330     C                   ENDSR
063900010607     C*----------------------------------------------------*
064000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
064100010607     C*----------------------------------------------------*
064200010607     C     IMPVAB        BEGSR
064300010607     C*
064400010607     C                   SETOFF                                       3132
064500030515     C*
064600010607     C                   MOVEL     datcor        VABAAS
064700040119     C                   MOVEL     datcor        VATAAS
064800010607     C                   MOVE      datcor        VABMGS
064900010607     C*
065000030715     C**********
065100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
065200030715     C**********
065300030715     C                   IF        wGiro = *zeros
065400040706     C*
065500040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
065600040706     C                   EVAL      depvinDTA = vinDTA
065700040113     C*
065800040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
065900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
066000040113     C                                      %trim(intcol(3))
066100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
066200031201     C     minu:maiu     XLATE     vindta        vindta
066300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
066400030715     C                   DOW       posDa <= %len(%trim(vindta))
066500030715     C*
066600030715     C* Gestisco il 1° campo
066700030715     C                   IF        i = 1
066800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
066900030715     C                   EVAL      posDa = 1
067000030715     C* Eseguo lo scan x trovare la fine del primo campo
067100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
067200030715     C* A questo punto "estraggo" il campo corrente
067300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
067400030715     C                                                    (posA-posDa))
067500030715     C* X i campi successivi al 1°
067600030715     C                   ELSE
067700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
067800030715     C                   EVAL      posDa = posA + 1
067900030715     C* Eseguo lo scan x trovare la fine del campo corrente
068000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
068100030715     C* Gestisco l'ultimo campo
068200030715     C                   IF        posA = *zeros
068300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
068400030715     C* A questo punto "estraggo" il campo corrente
068500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
068600030715     C                                                    (posA-posDa))
068700030715     C                   LEAVE
068800030715     C                   ELSE
068900030715     C* A questo punto "estraggo" il campo corrente
069000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
069100030715     C                                                    (posA-posDa))
069200030715     C                   ENDIF
069300030715     C                   ENDIF
069400030715     C* Incremento il contatore d campo
069500030715     C                   EVAL      i = i +1
069600030715     C                   ENDDO
069700030715     C*
069800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
069900030715     C* delimitazione testo
070000030715     C                   EVAL      i = 1
070100030715     C                   DOW       i < %elem(SkSplitFLD)
070200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
070300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
070400030715     C                   EVAL      i = i + 1
070500030715     C                   ENDDO
070600030715     C                   EVAL      wGiro = 1
070700040706     C* Ripristino la 1° riga originale
070800040706     C                   EVAL      vinDTA = depvinDTA
070900030715     C                   ELSE
071000030715     C                   EVAL      wGiro = 2
071100031204     C**********
071200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
071300031204     C* d separatore campo
071400031204     C**********
071500031204     C                   EVAL      vindta = %trim(vindta)
071600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
071700031204     C                   EVAL      vindta = %subst(vindta:2)
071800031204     C                   ENDDO
071900031204     C*
072000031204     C                   Z-ADD     *zeros        lunghInput        4 0
072100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
072200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
072300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
072400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
072500031204     C                   ENDDO
072600030715     C**********
072700030715     C* Effettuo lo split del campo dati d input
072800030715     C**********
072900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
073000030715     C                   DOW       posDa <= %len(%trim(vindta))
073100030715     C*
073200030715     C* Gestisco il 1° campo
073300030715     C                   IF        i = 1
073400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
073500030715     C                   EVAL      posDa = 1
073600030715     C* Eseguo lo scan x trovare la fine del primo campo
073700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
073800030715     C* A questo punto "estraggo" il campo corrente
073900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
074000030715     C                                                    (posA-posDa))
074100030715     C* X i campi successivi al 1°
074200030715     C                   ELSE
074300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
074400030715     C                   EVAL      posDa = posA + 1
074500031204     C* Verifico che nn vi sia il campo nn valorizzato
074600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
074700031204     C* Se campo nn valorizzato skippo al prossimo
074800031204     C                   EVAL      SkSplitCSV(i) = *blanks
074900031204     C                   EVAL      posA  = posA + 1
075000031204     C                   ELSE
075100030715     C* Eseguo lo scan x trovare la fine del campo corrente
075200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075300030715     C* Gestisco l'ultimo campo
075400030715     C                   IF        posA = *zeros
075500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
075600030715     C* A questo punto "estraggo" il campo corrente
075700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
075800030715     C                                                    (posA-posDa))
075900030715     C                   LEAVE
076000030715     C                   ELSE
076100030715     C* A questo punto "estraggo" il campo corrente
076200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
076300030715     C                                                    (posA-posDa))
076400030715     C                   ENDIF
076500031204     C                   ENDIF
076600030715     C                   ENDIF
076700030715     C* Incremento il contatore d campo
076800030715     C                   EVAL      i = i +1
076900030715     C                   ENDDO
077000030715     C*
077100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
077200030715     C* delimitazione testo
077300030715     C                   EVAL      i = 1
077400030715     C                   DOW       i < %elem(SkSplitCSV)
077500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
077600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
077700030715     C                   EVAL      i = i + 1
077800030715     C                   ENDDO
077900030715     C*
078000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
078100030715     C                   EVAL      i = 1
078200030715     C                   DOW       i < %elem(SkSplitFLD)
078300030715     C*
078400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
078500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
078600030715     C                             SkSplitCSV(i) <> *blanks
078700030721     C*** ATB
078800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
078900030721     C                   EVAL      VABATB = SkSplitCSV(i)
079000030721     C                   ENDIF
079100030715     C*** RSD
079200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
079300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
079400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
079500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
079600030715     C* ==
079700030715     C                   ENDIF
079800030721     C*** RD2
079900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
080000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
080100030721     C                   ENDIF
080200030715     C*** IND
080300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
080400030715     C                   EVAL      VABIND = SkSplitCSV(i)
080500030715     C                   ENDIF
080600030715     C*** LOD
080700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
080800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
080900030715     C                   ENDIF
081000030715     C*** PRD
081100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
081200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
081300030715     C                   ENDIF
081400030721     C*** NZD
081500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
081600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
081700030721     C                   ENDIF
081800030715     C*** RMA
081900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
082000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
082100030715     C                   ENDIF
082200030715     C*** NOT
082300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
082400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
082500030715     C                   ENDIF
082600030715     C*** NT2
082700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
082800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
082900030715     C                   ENDIF
083000030715     C*** VCA
083100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
083200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
083300030715     C                   ENDIF
083400030715     C*** VAS
083500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
083600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
083700030715     C                   ENDIF
083800030715     C*** FFD
083900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
084000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
084100030715     C                   ENDIF
084200030721     C*** GC1
084300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
084400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
084500030721     C                   ENDIF
084600030721     C*** GC2
084700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
084800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
084900030721     C                   ENDIF
085000030721     C*** TSP
085100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
085200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
085300030721     C                   ENDIF
085400030721     C*** NAS
085500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
085600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
085700030721     C                   ENDIF
085800030721     C*** TIC
085900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
086000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
086100030721     C                   ENDIF
086200030721     C*** GCA
086300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
086400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
086500030721     C                   ENDIF
086600030721     C*** XCO
086700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
086800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
086900030721     C                   ENDIF
087000030721     C*** CTM
087100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
087200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
087300030721     C                   ENDIF
087400030721     C*** TCR
087500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
087600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
087700030721     C                   ENDIF
087800030721     C*** CTS
087900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
088000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
088100030721     C                   ENDIF
088200030721     C*** FTM
088300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
088400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
088500030721     C                   ENDIF
088600030721     C*** VAD
088700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
088800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
088900030721     C                   ENDIF
089000030721     C*** GMA
089100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
089200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
089300030721     C                   ENDIF
089400030721     C*** GGA
089500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
089600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
089700030721     C                   ENDIF
089800030721     C*** GVA
089900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
090000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
090100030721     C                   ENDIF
090200030721     C*** TC1
090300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
090400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
090500030721     C                   ENDIF
090600030721     C*** TC2
090700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
090800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
090900030721     C                   ENDIF
091000030721     C*** SCL
091100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
091200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
091300030721     C                   ENDIF
091400030721     C*** RMO
091500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
091600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
091700030721     C                   ENDIF
091800030721     C*** NMO
091900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
092000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
092100030721     C                   ENDIF
092200040719     C*** CBO
092300040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
092400061211     C                   EVAL      VABCBO = SkSplitCSV(i)
092500040719     C                   ENDIF
092600030721     C*
092700040706     C* Reperisco quindi i campi numerici...
092800040706     C*** NSP
092900040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
093000040706     C                   EVAL      PiStr=SkSplitCSV(i)
093100040706     C                   EXSR      CHKNUM
093200040706     C                   IF        PiInt=*on
093300040706     C                   Z-ADD     PiVal         VABNSP
093400040706     C                   ELSE
093500040706     C                   SETON                                        31
093600040706     C                   EVAL      VABNSP = *zeros
093700040706     C                   EVAL      vinmsg = %trimr(vinmsg)
093800040706     C                             + ' ' + 'VABNSP'
093900040706     C                   ENDIF
094000040706     C                   ENDIF
094100040706     C*** RMN
094200040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
094300040706     C                   EVAL      PiStr=SkSplitCSV(i)
094400040706     C                   EXSR      CHKNUM
094500040706     C                   IF        PiInt=*on
094600040706     C                   Z-ADD     PiVal         VABRMN
094700040706     C                   ELSE
094800040706     C                   SETON                                        32
094900040706     C                   EVAL      VABRMN = 1
095000040706     C                   EVAL      vinmsg = %trimr(vinmsg)
095100040706     C                             + ' ' + 'VABRMN'
095200040706     C                   ENDIF
095300040706     C                   ENDIF
095400030721     C*** CAD
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
095600030721     C                   EVAL      PiStr=SkSplitCSV(i)
095700030721     C                   EXSR      CHKNUM
095800030721     C                   IF        PiInt=*on
095900030721     C                   Z-ADD     PiVal         Num5_0
096000030721     C                   MOVEL(p)  Num5_0        VABCAD
096100030721     C                   ELSE
096200030721     C                   SETON                                        32
096300030721     C                   EVAL      VABCAD = *zeros
096400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096500030721     C                             + ' ' + 'VABCAD'
096600030721     C                   ENDIF
096700030721     C                   ENDIF
096800030721     C*** NCL
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
097000030721     C                   EVAL      PiStr=SkSplitCSV(i)
097100030721     C                   EXSR      CHKNUM
097200030721     C                   IF        PiInt=*on
097300030721     C                   Z-ADD     PiVal         VABNCL
097400030721     C                   ELSE
097500030721     C                   SETON                                        32
097600030721     C                   EVAL      VABNCL = *zeros
097700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097800040706     C                             + ' ' + 'VABNCL'
097900030721     C                   ENDIF
098000030721     C                   ENDIF
098100030721     C*** PKB
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
098300030721     C                   EVAL      PiStr=SkSplitCSV(i)
098400030721     C                   EXSR      CHKNUM
098500030721     C                   IF        PiNum=*on
098600030721     C                   Z-ADD     PiVal         VABPKB
098700030721     C                   ELSE
098800030721     C                   SETON                                        32
098900030721     C                   EVAL      VABPKB = *zeros
099000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099100030721     C                             + ' ' + 'VABPKB'
099200030721     C                   ENDIF
099300030721     C                   ENDIF
099400030721     C*** CAS
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
099600030721     C                   EVAL      PiStr=SkSplitCSV(i)
099700030721     C                   EXSR      CHKNUM
099800030721     C                   IF        PiNum=*on
099900030721     C                   Z-ADD     PiVal         VABCAS
100000030721     C                   ELSE
100100030721     C                   SETON                                        32
100200030721     C                   EVAL      VABCAS = *zeros
100300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100400030721     C                             + ' ' + 'VABCAS'
100500030721     C                   ENDIF
100600030721     C                   ENDIF
100700030721     C*** CCM
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
100900030721     C                   EVAL      PiStr=SkSplitCSV(i)
101000030721     C                   EXSR      CHKNUM
101100030721     C                   IF        PiInt=*on      AND
101200030721     C                             PiVal<=9999999 AND
101300030721     C                             PiVal>*zeros
101400030721     C                   Z-ADD     PiVal         VABCCM
101500040119     C                   Z-ADD     PiVal         VATCCM
101600030721     C                   ELSE
101700030721     C                   SETON                                        32
101800030721     C                   EVAL      VABCCM = *zeros
101900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102000040119     C                             + ' ' + 'VABCCM VATCCM'
102100030721     C                   ENDIF
102200030721     C                   ENDIF
102300030721     C*** LNP
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
102500030721     C                   EVAL      PiStr=SkSplitCSV(i)
102600030721     C                   EXSR      CHKNUM
102700030721     C                   IF        PiInt=*on    AND
102800030721     C                             PiVal<=999   AND
102900030721     C                             PiVal>*zeros
103000030721     C                   Z-ADD     PiVal         VABLNP
103100040119     C                   Z-ADD     PiVal         VATLNP
103200030721     C                   ELSE
103300030721     C                   SETON                                        32
103400030721     C                   EVAL      VABLNP = *zeros
103500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103600040119     C                             + ' ' + 'VABLNP VATLNP'
103700030721     C                   ENDIF
103800030721     C                   ENDIF
103900030721     C*** NRS
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
104100030721     C                   EVAL      PiStr=SkSplitCSV(i)
104200030721     C                   EXSR      CHKNUM
104300030721     C                   IF        PiInt=*on AND
104400030721     C                             PiVal<=99
104500030721     C                   Z-ADD     PiVal         VABNRS
104600030721     C                   ELSE
104700030721     C                   SETON                                        32
104800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104900030721     C                             + ' ' + 'VABNRS'
105000030721     C                   ENDIF
105100030721     C                   ENDIF
105200030721     C*** LNA
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
105400030721     C                   EVAL      PiStr=SkSplitCSV(i)
105500030721     C                   EXSR      CHKNUM
105600030721     C                   IF        PiInt=*on    AND
105700030721     C                             PiVal<=999   AND
105800030721     C                             PiVal>*zeros
105900030721     C                   Z-ADD     PiVal         VABLNA
106000030721     C                   ELSE
106100030721     C                   SETON                                        32
106200030721     C                   EVAL      VABLNA = *zeros
106300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106400030721     C                             + ' ' + 'VABLNA'
106500030721     C                   ENDIF
106600030721     C                   ENDIF
106700030721     C*** CTR
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
106900030721     C                   EVAL      PiStr=SkSplitCSV(i)
107000030721     C                   EXSR      CHKNUM
107100030721     C                   IF        PiInt=*on  AND
107200030721     C                             PiVal<=999
107300030721     C                   Z-ADD     PiVal         VABCTR
107400030721     C                   ELSE
107500030721     C                   SETON                                        32
107600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107700030721     C                             + ' ' + 'VABCTR'
107800030721     C                   ENDIF
107900030721     C                   ENDIF
108000030721     C*** IAS
108100080207     C**!!!              IF        %trim(SkSplitFLD(i)) = 'VABIAS'
108200080207     C**!!!              EVAL      PiStr=SkSplitCSV(i)
108300080207     C**!!!              EXSR      CHKNUM
108400080207     C**!!!              IF        PiNum=*on
108500080207     C**!!!              Z-ADD     PiVal         VABIAS
108600080207     C**!!!              ELSE
108700080207     C**!!!              SETON                                        32
108800080207     C**!!!              EVAL      vinmsg = %trimr(vinmsg)
108900080207     C**!!!                        + ' ' + 'VABIAS'
109000080207     C**!!!              ENDIF
109100080207     C**!!!              ENDIF
109200030721     C*** VLB
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
109400030721     C                   EVAL      PiStr=SkSplitCSV(i)
109500030721     C                   EXSR      CHKNUM
109600030721     C                   IF        PiNum=*on
109700030721     C                   Z-ADD     PiVal         VABVLB
109800030721     C                   ELSE
109900030721     C                   SETON                                        32
110000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110100030721     C                             + ' ' + 'VABVLB'
110200030721     C                   ENDIF
110300030721     C                   ENDIF
110400030721     C*** QFT
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
110600030721     C                   EVAL      PiStr=SkSplitCSV(i)
110700030721     C                   EXSR      CHKNUM
110800030721     C                   IF        PiNum=*on
110900030721     C                   Z-ADD     PiVal         VABQFT
111000030721     C                   ELSE
111100030721     C                   SETON                                        32
111200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111300030721     C                             + ' ' + 'VABQFT'
111400030721     C                   ENDIF
111500030721     C                   ENDIF
111600030721     C*** NCD
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
111800030721     C                   EVAL      PiStr=SkSplitCSV(i)
111900030721     C                   EXSR      CHKNUM
112000030721     C                   IF        PiInt=*on      AND
112100030721     C                             PiVal<=9999999
112200030721     C                   Z-ADD     PiVal         VABNCD
112300030721     C                   ELSE
112400030721     C                   SETON                                        32
112500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112600030721     C                             + ' ' + 'VABNCD'
112700030721     C                   ENDIF
112800030721     C                   ENDIF
112900030721     C*** NCA
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
113100030721     C                   EVAL      PiStr=SkSplitCSV(i)
113200030721     C                   EXSR      CHKNUM
113300030721     C                   IF        PiInt=*on      AND
113400030721     C                             PiVal<=9999999
113500030721     C                   Z-ADD     PiVal         VABNCA
113600030721     C                   ELSE
113700030721     C                   SETON                                        32
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABNCA'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** ZNC
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiInt=*on AND
114700030721     C                             PiVal<=99
114800030721     C                   Z-ADD     PiVal         VABZNC
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200030721     C                             + ' ' + 'VABZNC'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** DCR
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on       AND
116000030721     C                             PiVal<=99999999
116100030721     C                   Z-ADD     PiVal         VABDCR
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABDCR'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** HCR
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiInt=*on   AND
117300030721     C                             PiVal<=9999
117400030721     C                   Z-ADD     PiVal         VABHCR
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABHCR'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** VMD
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiNum=*on
118600030721     C                   Z-ADD     PiVal         VABVMD
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119000030721     C                             + ' ' + 'VABVMD'
119100030721     C                   ENDIF
119200030721     C                   ENDIF
119300030721     C*** ANT
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
119500030721     C                   EVAL      PiStr=SkSplitCSV(i)
119600030721     C                   EXSR      CHKNUM
119700030721     C                   IF        PiInt=*on        AND
119800030721     C                             PiVal<=999999999
119900030721     C                   Z-ADD     PiVal         VABANT
120000030721     C                   ELSE
120100030721     C                   SETON                                        32
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300030721     C                             + ' ' + 'VABANT'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** CMO
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiInt=*on
121100030721     C                   Z-ADD     PiVal         Num5_0
121200030721     C                   MOVEL(p)  Num5_0        VABCMO
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      VABCMO = *zeros
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABCMO'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000040706     C*
122100040719     C*** VATNOT
122200040719     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
122300040719     C                   EVAL      VATNOT = SkSplitCSV(i)
122400040719     C                   ENDIF
122500080131     C*
122600080131     C*******
122700080131     C* Forzatura particolari dentro ciclo
122800080131     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1GC1'
122900080131     C                   IF        SkSplitCSV(i) = 'A'
123000080131     C                   EVAL      VABTC1 = 'A'
123100080131     C                   ENDIF
123200080328     C                   IF        SkSplitCSV(i) = 'S'
123300080328     C                   EVAL      VABFFD = 'S'
123400080328     C                   ENDIF
123500080131     C                   IF        SkSplitCSV(i) = 'M'
123600080131     C                   EVAL      %subst(VABGC1:2:1) = 'P'
123700080131     C                   ENDIF
123800080131     C                   IF        SkSplitCSV(i) = 'P'
123900080131     C                   EVAL      %subst(VABGC1:2:1) = 'M'
124000080131     C                   ENDIF
124100080131     C                   ENDIF
124200080131     C*******
124300030715     C*
124400030715     C                   ENDIF
124500030715     C                   EVAL      i = i + 1
124600030715     C                   ENDDO
124700040706     C*
124800040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
124900040706     C* Reperisco la provincia dal CAP e dalla località
125000040706     C                   IF        VABCAD <> *blanks
125100040706     C                   CLEAR                   TISI95DS
125200040706     C                   EVAL      I95TCN = '3'
125300040706     C                   Z-ADD     datcor        I95DAT
125400040706     C                   EVAL      I95CAP = VABCAD
125500040706     C                   EVAL      I95LOC = VABLOD
125600040706     C                   CALL      'TISI95R'
125700040706     C                   PARM                    TISI95DS
125800040706     C                   EVAL      VABPRD = O95PRV
125900040706     C                   ENDIF
126000061211     C*******
126100061211     C* Forzatura particolare: il numero spedizione bolla corrisponde al "chi sono"
126200061211     C* (ovviamente sono tutte spedizioni mono collo)
126300061211     C                   EVAL      PiStr=VATNOT
126400061211     C                   EXSR      CHKNUM
126500061211     C                   IF        PiNum=*on
126600061211     C                   Z-ADD     PiVal         VABNSP
126700061211     C                   ENDIF
126800061211     C*******
126900040719     C*
127000040719     C* Se NSP nn valorizzato considero errore bloccante in traduzione
127100040719     C                   IF        VABNSP = *zeros
127200040719     C                   SETON                                        31
127300040719     C                   ENDIF
127400040719     C*
127500040719     C* Se LNP nn valorizzato considero errore bloccante in traduzione
127600040719     C                   IF        VABLNP = *zeros
127700040719     C                   SETON                                        31
127800040719     C                   ENDIF
127900040706     C*
128000040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
128100040706     C* d valorizzarli sempre entrambi
128200040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
128300040706     C                   Z-ADD     VABNSP        VABRMN
128400040706     C                   ENDIF
128500040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
128600040706     C                   MOVEL     VABRMN        VABRMA
128700040706     C                   ENDIF
128800040113     C*
128900040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
129000040113     C                   IF        VABCAS > *zeros
129100110124     C***                EVAL      VABTIC = 'BM'
129200110124     C                   EVAL      VABTIC = *blanks
129300040119     C                   EVAL      VABVCA = 'EUR'
129400040119     C                   IF        VABCBO = '1'
129500040113     C                   EVAL      VABCBO = '4'
129600040119     C                   ENDIF
129700040119     C                   IF        VABCBO = '2'
129800040119     C                   EVAL      VABCBO = '6'
129900040119     C                   ENDIF
130000040113     C                   ENDIF
130100020204     C*
130200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
130300020204     C                   EXSR      CHKIMPDIV
130400030715     C                   ENDIF
130500020204     C*
130600010607     C                   ENDSR
130700010607     C*----------------------------------------------------*
130800010601
130900020204
131000020204     C*----------------------------------------------------*
131100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
131200020204     C*----------------------------------------------------*
131300020204     C     CHKIMPDIV     BEGSR
131400020204     C*
131500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
131600020204     C                   Z-ADD     *zeros        wrkDec            9 9
131700020204     C*
131800020204     C* Come prima cosa effettuo considerazioni sulla divisa
131900020204     C                   IF        vabIAS > *zeros
132000020204     C                   IF        vabVAS <> 'EUR'
132100020204     C                   EVAL      vabVAS =  'ITL'
132200020204     C                   ENDIF
132300020204     C                   ENDIF
132400020204     C*
132500020204     C                   IF        vabCAS > *zeros
132600020204     C                   IF        vabVCA <> 'EUR'
132700020204     C                   EVAL      vabVCA =  'ITL'
132800020204     C                   ENDIF
132900020204     C                   ENDIF
133000020204     C*
133100020204     C                   IF        vabVMD > *zeros
133200020204     C                   IF        vabVAD <> 'EUR'
133300020204     C                   EVAL      vabVAD =  'ITL'
133400020204     C                   ENDIF
133500020204     C                   ENDIF
133600020204     C*
133700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
133800020204     C                   Z-ADD     vabIAS        wrkDec
133900020204     C                   IF        wrkDec > *zeros
134000020204     C                   IF        vabVAS = 'ITL'
134100020204     C                   EVAL      vabIAS = *zeros
134200020204     C                   ENDIF
134300020204     C                   ENDIF
134400020204     C*
134500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
134600020204     C                   Z-ADD     vabCAS        wrkDec
134700020204     C                   IF        wrkDec > *zeros
134800020204     C                   IF        vabVCA = 'ITL'
134900020204     C                   EVAL      vabCAS = *zeros
135000020204     C                   ENDIF
135100020204     C                   ENDIF
135200020204     C*
135300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
135400020204     C                   Z-ADD     vabVMD        wrkDec
135500020204     C                   IF        wrkDec > *zeros
135600020204     C                   IF        vabVAD = 'ITL'
135700020204     C                   EVAL      vabVMD = *zeros
135800020204     C                   ENDIF
135900020204     C                   ENDIF
136000020204     C*
136100020204     C                   ENDSR
136200020204     C***
136300020204
136400010330
136500010330
136600010330     C*----------------------------------------------------*
136700010330     C*  CONTROLLO NUMERICITA' CAMPI
136800010330     C*----------------------------------------------------*
136900010330     C     CHKNUM        BEGSR
137000010330     C*
137100010606     C                   IF        PiDecChr = *blanks
137200030715     C                   EVAL      PiDecChr = CharNUM
137300010606     C                   ENDIF
137400010606     C*
137500010606     C                   CALL(e)   'ISNUMERIC'
137600010330     C                   PARM                    PiStr            30
137700010606     C                   PARM                    PiDecChr          1
137800010330     C                   PARM      *ZEROS        PiVal            30 9
137900010330     C                   PARM      '0'           PiInt             1
138000010330     C                   PARM      '0'           PiNum             1
138100010330     C                   IF        %error
138200010606     C                   EVAL      PiNum=*off
138300010330     C                   ENDIF
138400010330     C*
138500010330     C                   ENDSR
138600010330     C***
138700010330
138800010601
138900010601
139000010601
139100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
139200010601     C     repfil        BEGSR
139300010601     C*
139400010601     C                   if        invfil = *zeros and
139500010601     C                             depfil > *zeros and
139600010629     C                             (vinflg = *blanks or
139700010629     C                              vinflg = *zeros)
139800010601     C
139900010601     C                   eval      invfil = depfil
140000010601     C                   endif
140100010601     C*
140200010601     C                   if        depfil <> invfil and
140300010601     C                             invfil > *zeros
140400010601     C                   eval      flgMulti = '1'
140500010601     C                   if        vinflg = *blanks
140600010601     C                   add       1             cntNonEl
140700010601     C                   endif
140800010601     C                   endif
140900010601     C*
141000010601     C                   if        vinflg = '2'
141100010601     C                   eval      flgStato = '2'
141200010601     C                   endif
141300010601     C*
141400010601     C                   ENDSR
141500010601     C***
141600010601
141700010601
141800010601
141900010330
142000040119      /TITLE Invio dei dati al punto operativo.
142100040119     C     invio         BEGSR
142200040119     C*
142300040119     C* 1° invio FIVAT
142400040119     C                   reset                   dscmz
142500040119     C                   move      vlrpoi        cmzdst
142600040119     C                   eval      cmzfld = 'FIVATWWR'
142700040119     C                   eval      cmzmbd = vlrhdl
142800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
142900040119     C***                if        prmfir = *blanks
143000040119     C                   eval      cmzfla = 'FIVAT00F'
143100040119     C                   eval      cmzmba = 'FIVAT00F'
143200040119     C***                else
143300040119     C***                eval      cmzfla = prmfir
143400040119     C***                eval      cmzmba = prmfir
143500040119     C***                endif
143600040119     C                   eval      cmznrr = *zeros
143700040119     C                   move      §ctrokvt      cmznrr
143800040119     C                   eval      cmzlba = vlrfl1
143900040119     C                   call(e)   'TIS711C'
144000040119     C                   parm                    dscmz
144100040119     C                   parm      *blanks       esito
144200040119     C                   if        %error
144300040119     C                             or cmzerr = '1'
144400040119     C                             or esito  = '1'
144500040119     C                   eval      wrkesito = '3'
144600040119     C                   else
144700040119     C*
144800040119     C* 2° invio FIVAB
144900040119     C                   reset                   dscmz
145000040119     C                   move      vlrpoi        cmzdst
145100040119     C                   eval      cmzfld = vlrfou
145200040119     C                   eval      cmzmbd = vlrhdl
145300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
145400040119     C***                if        prmfir = *blanks
145500040119     C                   eval      cmzfla = 'FIVAB00F'
145600040119     C                   eval      cmzmba = 'FIVAB00F'
145700040119     C***                else
145800040119     C***                eval      cmzfla = prmfir
145900040119     C***                eval      cmzmba = prmfir
146000040119     C***                endif
146100040119     C                   eval      cmznrr = *zeros
146200040119     C                   move      §ctrokvb      cmznrr
146300040119     C                   eval      cmzlba = vlrfl1
146400040119     C                   call(e)   'TIS711C'
146500040119     C                   parm                    dscmz
146600040119     C                   parm      *blanks       esito
146700040119     C                   if        %error
146800040119     C                             or cmzerr = '1'
146900040119     C                             or esito  = '1'
147000040119     C                   eval      wrkesito = '3'
147100040119     C                   endif
147200040119     C                   endif
147300040119     C*
147400040119     C                   ENDSR
147500040119     C***
147600010601
147700010601
147800010601
147900010601
148000010601      /TITLE Invio dei dati al punto operativo.
148100010601     C     opeini        BEGSR
148200010601     C*
148300010601     C* Inizializzo flag e contatori operativi
148400010601     C                   movel     '0'           flgGiro           1
148500010601     C                   movel     '0'           flgMulti          1
148600010601     C                   movel     '1'           flgStato          1
148700010615     C                   movel     '0'           flgOk             1
148800010601     C                   z-add     *zeros        cntNonEl         10 0
148900010601     C                   z-add     *zeros        depfil            3 0
149000010601     C                   z-add     *zeros        invfil            3 0
149100010601     C*
149200010601     C                   ENDSR
149300010601     C***
149400010601
149500010601
149600050421
149700050421
149800070411
149900070411     C     *pssr         BEGSR
150000070411     C*
150100070411     C                   if        %open(tivin00r)
150200070411     C                   close     tivin00r
150300070411     C                   endif
150400070411     C                   if        %open(fivabwwr)
150500070411     C                   close     fivabwwr
150600070411     C                   endif
150700070411     C                   if        %open(fivatwwr)
150800070411     C                   close     fivatwwr
150900070411     C                   endif
151000070411     C*
151100070411     C* Effettuo la chiamata al CLLE preposto
151200070411     C                   call(e)   'TITVVTC'
151300070411     C                   parm                    parccm
151400070411     C                   parm                    parmbr
151500070411     C                   parm      '2'           paropz
151600070411     C*
151700070411     C                   eval      wrkesito = '2'
151800070411     C*
151900070411     C                   seton                                        LR
152000070411     C*
152100070411     C                   ENDSR     '*CANCL'
152200070411     C***
152300070411
152400050421
152500010601
152600010330
152700010330
152800000613     C     *inzsr        BEGSR
152900990910     C*
153000990910     C     *entry        plist
153100990920     C                   parm                    tivlrds
153200990921     C                   parm      wrkesito      esito
153300000724     C                   parm                    prmlit
153400000710     C                   parm                    prmfir
153500010330     C*
153600010330     C* CALCOLA LA DATA CORRENTE
153700010330     C                   time                    wn14             14 0
153800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
153900010330     C                   z-add     wn8           g08dat
154000010330     C                   z-add     *zeros        g08inv
154100010330     C                   movel     '0'           g08err
154200010330     C                   call      'XSRDA8'
154300010330     C                   parm                    wlbda8
154400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
154500000613     C*
154600000613     C                   ENDSR
154700000613     C***
154800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
154900080123FILLER1;VATNOT;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNT2;VABRD2;
155000080131VABRMA;VABNCL;VABPKB;VABCAS;VABIAS;VABNOT;VABTC1GC1
155100040113
