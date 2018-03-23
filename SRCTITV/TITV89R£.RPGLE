000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800040119     Ftitv89p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100040119     Ftitv89ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400000313     D*
001500000613     D*------------
001600000613     D* COMANDI
001700000613     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900040113     D*------------
002000040113     D* SCHIEDE A PROGRAMMA
002100040113     D*------------
002200040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000010330     D esito           s              1
003100010330     D prmlit          s             10
003200010330     D prmfir          s             10
003300010330     D wrkesito        s                   like(esito)
003400010330     D wrkdata         s               d
003500010330     D wrkora          s               t
003600010330     D rrnum           s              6  0 INZ(*zeros)
003700010330     D recko           s            150    INZ(*blanks)
003800011113     D depcmd          s            150    INZ(*blanks)
003900040119     D wAlfa2          s              2    INZ(*blanks)
004000040119     D wAlfa4          s              4    INZ(*blanks)
004100040119     D wNumCollo       s              2  0 INZ(*zeros)
004200040119     D wNumColloA      s              2    INZ(*blanks)
004300040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004400040119     D depnot          s                   LIKE(VATNOT) INZ(*blanks)
004500040119     D parccm          s              8    INZ(*blanks)
004600040119     D parmbr          s             10    INZ(*blanks)
004700040119     D paropz          s              1    INZ(*blanks)
004800040119     D chkcall         s              1    INZ(*blanks)
004900030715     D*------------------
005000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005100030715     D*------------------
005200030715     D SkSplitFLD      S             10    DIM(100)
005300030715     D SkSplitCSV      S            256    DIM(100)
005400030715     D CharCSV         S              1
005500030715     D CharTXT         S              1
005600030715     D CharNUM         S              1
005700030715     D posDa           S              3  0 INZ(*zeros)
005800030715     D posA            S              3  0 INZ(*zeros)
005900030715     D i               s              3  0 INZ(1)
006000030715     D wGiro           s              1  0 INZ(*zeros)
006100030822     D*------------------
006200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006300030822     D*------------------
006400030822     D posDaDft        S              3  0 INZ(*zeros)
006500030822     D posADft         S              3  0 INZ(*zeros)
006600030822     D j               s              3  0 INZ(1)
006700030822     D wGiroDft        s              1  0 INZ(*zeros)
006800010330     D*------------------
006900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007000010330     D*------------------
007100010330     D WLBDA8          DS                  INZ
007200010330     D  G08DAT                 1      8  0
007300010330     D  G08INV                 9     16  0
007400010330     D  G08ERR                17     17
007500010330     D  G08TGI                18     22  0
007600031201     D*------------------
007700031201     D* Costanti
007800031201     D*------------------
007900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008100010330
008200010330
008300010330
008400990915     C                   time                    wrkdata
008500990915     C                   time                    wrkora
008600000913     C                   reset                   rrnum
008700990921     C                   reset                   esito
008800990921     C                   reset                   wrkesito
008900010601     C*
009000010601     C                   exsr      opeini
009100010605     C                   exsr      rwvab
009200010601     C*
009300010601     C                   seton                                        lr
009400010601
009500010601
009600010601
009700010601     C*--------------------------------------------------------
009800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009900010601     C*--------------------------------------------------------
010000010601     C     PREELA        BEGSR
010100010601     C*
010200010601     C* SE OCCORRE SPEDIRE IN FILIALE
010300010601     C                   if        invfil <> *zeros and
010400010601     C                             flgGiro = '0'
010500010601     C*
010600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010700010601     C                   eval      flgGiro = '1'
010800000724     C*
010900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011100010601     C     invfil        chain     azorg01l
011200000724     C                   if        %found
011300000724     C                   movel(p)  CMD(1)        depcmd
011400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011500000724     C*
011600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011700011113     C                   Z-ADD     150           LENGH            15 5
011800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011900000724     C                   PARM                    depcmd
012000000724     C                   PARM                    LENGH
012100000724     C*
012200000724     C                   endif
012300000616     C*
012400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012500000616     C                   movel(p)  CMD(2)        depcmd
012600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012700000616     C*
012800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012900011113     C                   Z-ADD     150           LENGH            15 5
013000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013100000616     C                   PARM                    depcmd
013200000616     C                   PARM                    LENGH
013300000613     C*
013400040119     C                   if        not %open(titv89ps)
013500040119     C                   open      titv89ps
013600030715     C                   except    testdett
013700000613     C                   endif
013800010601     C*
013900010601     C                   endif
014000010601     C*
014100010601     C                   ENDSR
014200010601     C***
014300010601
014400010601
014500010601
014600010601
014700010601     C*--------------------------------------------------------
014800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014900010601     C*--------------------------------------------------------
015000010601     C     ENDELA        BEGSR
015100990915     C*
015200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015300040119     C*
015400040119     C* Effettuo la chiamata al CLLE preposto
015500040119     C                   call(e)   'TITVVTC'
015600040119     C                   parm                    parccm
015700040119     C                   parm                    parmbr
015800040119     C                   parm      '2'           paropz
015900000613     C*
016000040119     C                   if        %open(titv89ps)
016100000616     C                   except    findett
016200040119     C                   close     titv89ps
016300000613     C                   endif
016400000616     C*
016500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016600011113     C                   Z-ADD     150           LENGH            15 5
016700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
016800000616     C                   PARM                    CMD(3)
016900000616     C                   PARM                    LENGH
017000000616     C*
017100010601     C                   ENDSR
017200010601     C***
017300010601
017400010601
017500010601
017600000613
017700000613     C*--------------------------------------------------------
017800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017900000613     C*--------------------------------------------------------
018000000621     C     STPR          BEGSR
018100000613     C*
018200040119     C                   if        not %open(titv89p)
018300040119     C   50              open      titv89p
018400990915     C                   endif
018500990915     C*
018600030715     C   50              except    riepilogo
018700990915     C*
018800040119     C                   if        %open(titv89p)
018900040119     C   50              close     titv89p
019000990914     C                   endif
019100000613     C*
019200000613     C                   ENDSR
019300000613     C***
019400000613
019500010601
019600010601
019700010330     C*--------------------------------------------------------
019800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
019900010330     C*--------------------------------------------------------
020000010605     C     RWVAB         BEGSR
020100010330     C*
020200010330     C                   if        not %open(tivin00r)
020300010330     C                   open      tivin00r
020400010330     C                   endif
020500021025     C                   if        not %open(fivabwwr)
020600021025     C                   open      fivabwwr
020700010330     C                   endif
020800040119     C*
020900040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021000040119     C                   exsr      prevat
021100040119     C*
021200040119     C                   if        chkcall = '0'
021300040119     C*
021400040119     C                   if        not %open(fivatwwr)
021500040119     C                   open      fivatwwr
021600040119     C                   endif
021700010330     C*
021800040119     C                   clear                   §CTROKVB          7 0
021900040119     C                   clear                   §CTROKVT          7 0
022000040119     C                   clear                   §CTRMO            7 0
022100040119     C                   clear                   §CTRNO            7 0
022200010330     C*
022300030822     C                   do        *HIVAL
022400010330     C*
022500030822     C                   read      tivin00r                               70
022600010618     C*
022700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
022800010618     C                   if        vinflg = '1'
022900010618     C                   eval      flgOk = '1'
023000010618     C                   endif
023100010618     C*
023200040316     C                   if        (vindta > *blanks AND
023300040316     C                              %trim(vindta) <> *all';') OR
023400040316     C                             wGiro = *zeros
023500010330     C                   add       1             rrnum
023600010330     C*
023700010601     C                   if        *in70 = *off and
023800010330     C                             (vinflg = *blanks
023900010330     C                              or vinflg = '0'
024000010330     C                              or vinflg = '2')
024100010330     C*
024200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
024300010711     C                   if        vinflg = *blanks or vinflg = '0'
024400010711     C                   clear                   vinmsg
024500010711     C                   endif
024600010601     C*
024700010330     C                   exsr      inzvar
024800010605     C                   exsr      impvab
024900010601     C*
025000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025100010605     C                   eval      depfil = VABLNP
025200010601     C                   exsr      repfil
025300010601     C                   if        depfil = invfil
025400021025     C                   if        vlrpoi = 999
025500030822     C                   move(P)   invfil        VABFGS
025600040119     C                   move(P)   invfil        VATFGS
025700021025     C                   else
025800030822     C                   move(P)   vlrpoi        VABFGS
025900040119     C                   move(P)   vlrpoi        VATFGS
026000021025     C                   endif
026100010601     C*
026200010601     C                   exsr      PREELA
026300010601     C*
026400010604     C* Ebbene...
026500010604     C*
026600040119     C  N31              add       1             §CTROKVB
026700040119     C   32              add       1             §CTRMO
026800040119     C   31              add       1             §CTRNO
026900030822     C                   if        wGiro = 2
027000040119     C                   if        depspe =  *zeros OR
027100040119     C                             depspe <> vabNSP
027200040119     C  N31              exsr      wrivab
027300040119     C  N31              eval      depspe = vabNSP
027400040119     C                   endif
027500040119     C                   endif
027600010604     C*
027700010604     C                   if        *in31 = *off and
027800010604     C                             *in32 = *off
027900010604     C                   eval      vinflg = '1'
028000010604     C                   else
028100010604     C                   eval      recko = vindta
028200010604     C                   except    rigadett
028300010604     C                   eval      vinflg = '2'
028400010604     C                   endif
028500010604     C                   endif
028600010601     C*
028700010604     C                   endif
028800010604     C*
028900010330     C                   else
029000010330     C                   eval      vinflg = '1'
029100010330     C                   endif
029200010601     C*
029300010601     C  N70              update    tivin000
029400010330     C*
029500030822     C  N70              enddo
029600040119     C*
029700040119     C                   endif                                                  (endif - chkcall)
029800010601     C*
029900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030000010601     C                   if        cntNonEl = *zeros or
030100010601     C                             flgMulti = '0'
030200010330     C* Se non ci sono record con errori ...
030300010601     C                   if        §ctrno = 0 and
030400010604     C                             §ctrmo = 0 and
030500010601     C                             flgStato <> '2'
030600010330     C* ... restituisco esito OK.
030700010330     C                   eval      wrkesito = '0'
030800010330     C                   else
030900040119     C                   if        §ctrokvb > 0
031000010330     C                   eval      wrkesito = '1'
031100010330     C                   else
031200010615     C                   if        flgOk = '0'
031300010615     C                   eval      wrkesito = '2'
031400010615     C                   else
031500010615     C                   eval      wrkesito = '6'
031600010615     C                   endif
031700010330     C                   endif
031800010330     C                   endif
031900010601     C                   else
032000010601     C                   eval      wrkesito = '9'
032100010601     C                   endif
032200010330     C*
032300010330     C                   if        %open(tivin00r)
032400010330     C                   close     tivin00r
032500010330     C                   endif
032600021025     C                   if        %open(fivabwwr)
032700021025     C                   close     fivabwwr
032800010330     C                   endif
032900040512     C                   if        %open(fivatwwr)
033000040512     C                   close     fivatwwr
033100040512     C                   endif
033200010601     C*
033300010601     C                   if        vlrpoi <> 999
033400010601     C                   eval      invfil = vlrpoi
033500010601     C                   endif
033600010330     C*
033700040119     C                   if        §ctrokvb > 0
033800010601     C                             and invfil > *zeros
033900010330     C                   exsr      invio
034000010330     C                   endif
034100010601     C*
034200010618     C                   if        flgGiro = '1'
034300010601     C                   exsr      endela
034400010618     C                   endif
034500010330     C*
034600010330     C                   ENDSR
034700010330     C***
034800040119
034900040119
035000040119
035100040119     C*----------------------------------------------------*
035200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035300040119     C*----------------------------------------------------*
035400040119     C     WRIVAB        BEGSR
035500040119     C*
035600040119     C                   write     fivab000                                     => scarico il VAB
035700040119     C*
035800041008     C* Se trattasi d "Disk C" eseguo la scrittura dei "CHI SONO" della spedizione corrente
035900041008     C                   if        %subst(VABCTM:1:1) = '7'
036000040119     C                   exsr      wrivat
036100041008     C                   endif
036200040119     C*
036300040119     C                   ENDSR
036400040119
036500040119
036600040119
036700040119     C*----------------------------------------------------*
036800040119     C*  SCARICAMENTO BUFFER RECORDS VAT
036900040119     C*----------------------------------------------------*
037000040119     C     WRIVAT        BEGSR
037100040119     C*
037200040119     C* Inizializzo progressivo collo su totale colli della spedizione
037300040119     C                   Z-ADD     *zeros        wNumCollo
037400040119     C*
037500040119     C* Salvo la parte costante del barcode relativa alla spedizoine corrente
037600040119     C                   EVAL      depnot = VATNOT
037700040119     C*
037800040119     C* Costriuisco <N> barcode a seconda del numero d colli presenti sulla bolla
037900040119     C                   DOW       wNumCollo < VABNCL
038000040119     C*** 4° parte VATNOT
038100040119     C                   EVAL      VATNOT = depnot
038200040119     C                   EVAL      wNumCollo = wNumCollo + 1
038300040119     C                   MOVE      wNumCollo     wNumColloA
038400040119     C                   EVAL      VATNOT = %trim(VATNOT)+wNumColloA
038500040119     C                   MOVE      VABNCL        wAlfa2
038600040119     C                   EVAL      VATNOT = %trim(VATNOT)+wAlfa2
038700040119     C*
038800040119     C                   write     FIVAT000
038900040119     C                   add       1             §CTROKVT
039000040119     C                   ENDDO
039100040119     C*
039200040119     C                   ENDSR
039300040119
039400040119
039500040119
039600040119     C*----------------------------------------------------*
039700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
039800040119     C*----------------------------------------------------*
039900040119     C     PREVAT        BEGSR
040000040119     C*
040100040119     C* Compongo il nome del membro da dare al FIVATWWR
040200040119     C                   eval      parmbr = vlrhdl
040300040119     C                   movel     'M'           parmbr
040400041008     C                   eval      parccm = vlrksc
040500040119     C                   eval      paropz = '1'
040600040119     C* Effettuo la chiamata al CLLE preposto
040700040119     C                   call(e)   'TITVVTC'
040800040119     C                   parm                    parccm
040900040119     C                   parm                    parmbr
041000040119     C                   parm                    paropz
041100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041200040119     C                   if        %error
041300040119     C                   movel     '1'           chkcall
041400040119     C                   else
041500040119     C                   movel     '0'           chkcall
041600040119     C                   endif
041700040119     C*
041800040119     C                   ENDSR
041900010601
042000010601
042100010601
042200010330     C*----------------------------------------------------*
042300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
042400010330     C*----------------------------------------------------*
042500040119     C     INZVAR        BEGSR
042600010330     C*
042700030822     C* Inizializzo variabili di wrk
042800020204     C                   Z-ADD     *zeros        Num5_0            5 0
042900040119     C                   MOVEL     *blanks       wAlfa2
043000040119     C                   MOVEL     *blanks       wAlfa4
043100030822     C*
043200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
043300030822     C                   CLEAR                   FIVAB000
043400040119     C                   CLEAR                   FIVAT000
043500030822     C                   CLEAR                   SkSplitCSV
043600030822     C                   EVAL      i = 1
043700030822     C                   EVAL      posDa = *zeros
043800030822     C                   EVAL      posA  = *zeros
043900030822     C*
044000030822     C* Reimposto i valori di default
044100030822     C                   EXSR      DEFCAM
044200030822     C*
044300010330     C                   ENDSR
044400010330     C*----------------------------------------------------*
044500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
044600010330     C*----------------------------------------------------*
044700010330     C     DEFCAM        BEGSR
044800040119     C*
044900040119     C* Imposto i valori d default a programma dei campi fondamentali
045000040119     C                   EVAL      VABCCM = 1200594
045100040119     C                   EVAL      VATCCM = 1200594
045200040119     C                   EVAL      VABLNP = 120
045300040119     C                   EVAL      VATLNP = 120
045400040119     C                   EVAL      VABCTR = 005
045500041008     C***                EVAL      VABCTM = '7Q'
045600040119     C                   EVAL      VABCBO = '1'
045700040119     C                   EVAL      VATTRC = 'E'
045800030822     C*
045900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
046000030715     C* e delimitatore testo.
046100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
046200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
046300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
046400030822     C*
046500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
046600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
046700030715     C                   SETON                                        50
046800030715     C                   ELSE
046900030715     C                   SETOFF                                       50
047000030715     C                   ENDIF
047100030822     C*
047200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
047300030822     C                   EVAL      posDaDft = 1
047400030822     C                   EVAL      posADft  = 0
047500030822     C                   EVAL      wGiroDft = 0
047600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
047700030822     C                             posDaDft > 0
047800030822     C*
047900030822     C* Gestisco il 1° giro
048000030822     C                   IF        wGiroDft = 0
048100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
048200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
048300030822     C* Incremento il contatore dei "giri"
048400030822     C                   EVAL      wGiroDft = 1
048500030822     C                   ELSE
048600030822     C                   EVAL      posDaDft = posADft
048700030822     C                   ENDIF
048800030822     C* Eseguo lo scan x trovare la fine del campo corrente
048900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
049000030822     C*
049100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
049200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
049300030822     C                   IF        posDaDft > 0 AND
049400030822     C                             posADft  > 0
049500030822     C* NCL
049600030822     C                   IF        %subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):1:3)
049900030822     C                             = 'NCL'
050000030822     C                   EVAL      PiStr=%trim(%subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050300030822     C                   EXSR      CHKNUM
050400030822     C                   IF        PiInt=*on
050500030822     C                   Z-ADD     PiVal         VABNCL
050600030822     C                   ENDIF
050700030822     C                   ENDIF
050800030822     C* CCM
050900030822     C                   IF        %subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):1:3)
051200030822     C                             = 'CCM'
051300030822     C                   EVAL      PiStr=%trim(%subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051600030822     C                   EXSR      CHKNUM
051700030822     C                   IF        PiInt=*on
051800030822     C                   Z-ADD     PiVal         VABCCM
051900040119     C                   Z-ADD     PiVal         VATCCM
052000030822     C                   ENDIF
052100030822     C                   ENDIF
052200030822     C* LNP
052300030822     C                   IF        %subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):1:3)
052600030822     C                             = 'LNP'
052700030822     C                   EVAL      PiStr=%trim(%subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053000030822     C                   EXSR      CHKNUM
053100030822     C                   IF        PiInt=*on
053200030822     C                   Z-ADD     PiVal         VABLNP
053300040119     C                   Z-ADD     PiVal         VATLNP
053400030822     C                   ENDIF
053500030822     C                   ENDIF
053600030822     C* NRS
053700030822     C                   IF        %subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):1:3)
054000030822     C                             = 'NRS'
054100030822     C                   EVAL      PiStr=%trim(%subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054400030822     C                   EXSR      CHKNUM
054500030822     C                   IF        PiInt=*on
054600030822     C                   Z-ADD     PiVal         VABNRS
054700030822     C                   ENDIF
054800030822     C                   ENDIF
054900030822     C* CTR
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'CTR'
055400030822     C                   EVAL      PiStr=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   EXSR      CHKNUM
055800030822     C                   IF        PiInt=*on
055900030822     C                   Z-ADD     PiVal         VABCTR
056000030822     C                   ENDIF
056100030822     C                   ENDIF
056200030822     C* PKB
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'PKB'
056700030822     C                   EVAL      PiStr=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   EXSR      CHKNUM
057100030822     C                   IF        PiNum=*on
057200030822     C                   Z-ADD     PiVal         VABPKB
057300030822     C                   ENDIF
057400030822     C                   ENDIF
057500030822     C* VLB
057600030822     C                   IF        %subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):1:3)
057900030822     C                             = 'VLB'
058000030822     C                   EVAL      PiStr=%trim(%subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058300030822     C                   EXSR      CHKNUM
058400030822     C                   IF        PiNum=*on
058500030822     C                   Z-ADD     PiVal         VABVLB
058600030822     C                   ENDIF
058700030822     C                   ENDIF
058800030822     C* QFT
058900030822     C                   IF        %subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):1:3)
059200030822     C                             = 'QFT'
059300030822     C                   EVAL      PiStr=%trim(%subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600030822     C                   EXSR      CHKNUM
059700030822     C                   IF        PiNum=*on
059800030822     C                   Z-ADD     PiVal         VABQFT
059900030822     C                   ENDIF
060000030822     C                   ENDIF
060100030822     C* CBO
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'CBO'
060600030822     C                   EVAL      VABCBO=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   ENDIF
061000030822     C* TSP
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'TSP'
061500030822     C                   EVAL      VABTSP=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   ENDIF
061900030822     C* VAS
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'VAS'
062400030822     C                   EVAL      VABVAS=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   ENDIF
062800030822     C* VCA
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'VCA'
063300030822     C                   EVAL      VABVCA=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* GCA
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'GCA'
064200030822     C                   EVAL      VABGCA=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* CTM
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'CTM'
065100030822     C                   EVAL      VABCTM=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* FFD
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'FFD'
066000030822     C                   EVAL      VABFFD=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* VAD
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'VAD'
066900030822     C                   EVAL      VABVAD=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* GMA
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'GMA'
067800030822     C                   EVAL      VABGMA=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* GGA
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'GGA'
068700030822     C                   EVAL      VABGGA=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* GVA
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'GVA'
069600030822     C                   EVAL      VABGVA=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* TC1
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'TC1'
070500030822     C                   EVAL      VABTC1=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* TC2
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'TC2'
071400030822     C                   EVAL      VABTC2=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* ...
071900030822     C                   ENDIF
072000030822     C                   ENDDO
072100020204     C*
072200010330     C                   ENDSR
072300010607     C*----------------------------------------------------*
072400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
072500010607     C*----------------------------------------------------*
072600010607     C     IMPVAB        BEGSR
072700010607     C*
072800010607     C                   SETOFF                                       3132
072900030515     C*
073000010607     C                   MOVEL     datcor        VABAAS
073100040119     C                   MOVEL     datcor        VATAAS
073200010607     C                   MOVE      datcor        VABMGS
073300010607     C*
073400030715     C**********
073500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073600030715     C**********
073700030715     C                   IF        wGiro = *zeros
073800040113     C*
073900040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
074000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
074100040113     C                                      %trim(intcol(3))
074200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
074300031201     C     minu:maiu     XLATE     vindta        vindta
074400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074500030715     C                   DOW       posDa <= %len(%trim(vindta))
074600030715     C*
074700030715     C* Gestisco il 1° campo
074800030715     C                   IF        i = 1
074900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075000030715     C                   EVAL      posDa = 1
075100030715     C* Eseguo lo scan x trovare la fine del primo campo
075200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075300030715     C* A questo punto "estraggo" il campo corrente
075400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075500030715     C                                                    (posA-posDa))
075600030715     C* X i campi successivi al 1°
075700030715     C                   ELSE
075800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
075900030715     C                   EVAL      posDa = posA + 1
076000030715     C* Eseguo lo scan x trovare la fine del campo corrente
076100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076200030715     C* Gestisco l'ultimo campo
076300030715     C                   IF        posA = *zeros
076400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
076500030715     C* A questo punto "estraggo" il campo corrente
076600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076700030715     C                                                    (posA-posDa))
076800030715     C                   LEAVE
076900030715     C                   ELSE
077000030715     C* A questo punto "estraggo" il campo corrente
077100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077200030715     C                                                    (posA-posDa))
077300030715     C                   ENDIF
077400030715     C                   ENDIF
077500030715     C* Incremento il contatore d campo
077600030715     C                   EVAL      i = i +1
077700030715     C                   ENDDO
077800030715     C*
077900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078000030715     C* delimitazione testo
078100030715     C                   EVAL      i = 1
078200030715     C                   DOW       i < %elem(SkSplitFLD)
078300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
078400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
078500030715     C                   EVAL      i = i + 1
078600030715     C                   ENDDO
078700030715     C                   EVAL      wGiro = 1
078800030715     C                   ELSE
078900030715     C                   EVAL      wGiro = 2
079000031204     C**********
079100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
079200031204     C* d separatore campo
079300031204     C**********
079400031204     C                   EVAL      vindta = %trim(vindta)
079500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
079600031204     C                   EVAL      vindta = %subst(vindta:2)
079700031204     C                   ENDDO
079800031204     C*
079900031204     C                   Z-ADD     *zeros        lunghInput        4 0
080000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
080200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
080300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080400031204     C                   ENDDO
080500030715     C**********
080600030715     C* Effettuo lo split del campo dati d input
080700030715     C**********
080800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080900030715     C                   DOW       posDa <= %len(%trim(vindta))
081000030715     C*
081100030715     C* Gestisco il 1° campo
081200030715     C                   IF        i = 1
081300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081400030715     C                   EVAL      posDa = 1
081500030715     C* Eseguo lo scan x trovare la fine del primo campo
081600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081700030715     C* A questo punto "estraggo" il campo corrente
081800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081900030715     C                                                    (posA-posDa))
082000030715     C* X i campi successivi al 1°
082100030715     C                   ELSE
082200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082300030715     C                   EVAL      posDa = posA + 1
082400031204     C* Verifico che nn vi sia il campo nn valorizzato
082500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
082600031204     C* Se campo nn valorizzato skippo al prossimo
082700031204     C                   EVAL      SkSplitCSV(i) = *blanks
082800031204     C                   EVAL      posA  = posA + 1
082900031204     C                   ELSE
083000030715     C* Eseguo lo scan x trovare la fine del campo corrente
083100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083200030715     C* Gestisco l'ultimo campo
083300030715     C                   IF        posA = *zeros
083400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083500030715     C* A questo punto "estraggo" il campo corrente
083600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083700030715     C                                                    (posA-posDa))
083800030715     C                   LEAVE
083900030715     C                   ELSE
084000030715     C* A questo punto "estraggo" il campo corrente
084100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084200030715     C                                                    (posA-posDa))
084300030715     C                   ENDIF
084400031204     C                   ENDIF
084500030715     C                   ENDIF
084600030715     C* Incremento il contatore d campo
084700030715     C                   EVAL      i = i +1
084800030715     C                   ENDDO
084900030715     C*
085000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085100030715     C* delimitazione testo
085200030715     C                   EVAL      i = 1
085300030715     C                   DOW       i < %elem(SkSplitCSV)
085400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
085500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
085600030715     C                   EVAL      i = i + 1
085700030715     C                   ENDDO
085800030715     C*
085900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086000030715     C                   EVAL      i = 1
086100030715     C                   DOW       i < %elem(SkSplitFLD)
086200030715     C*
086300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
086400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
086500030715     C                             SkSplitCSV(i) <> *blanks
086600030721     C*** ATB
086700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
086800030721     C                   EVAL      VABATB = SkSplitCSV(i)
086900030721     C                   ENDIF
087000030715     C*** RSD
087100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
087200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
087300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
087400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
087500030715     C* ==
087600030715     C                   ENDIF
087700030721     C*** RD2
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
087900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030715     C*** IND
088200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
088300030715     C                   EVAL      VABIND = SkSplitCSV(i)
088400030715     C                   ENDIF
088500030715     C*** LOD
088600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
088700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
088800030715     C                   ENDIF
088900030715     C*** PRD
089000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
089100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
089200030715     C                   ENDIF
089300030721     C*** NZD
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
089500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030715     C*** RMA
089800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
089900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
090000030715     C                   ENDIF
090100030715     C*** NOT
090200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
090300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
090400030715     C                   ENDIF
090500030715     C*** NT2
090600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
090700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
090800030715     C                   ENDIF
090900030715     C*** VCA
091000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
091100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
091200030715     C                   ENDIF
091300030715     C*** VAS
091400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
091500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
091600030715     C                   ENDIF
091700030715     C*** FFD
091800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
091900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
092000030715     C                   ENDIF
092100030721     C*** GC1
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
092300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
092400030721     C                   ENDIF
092500030721     C*** GC2
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
092700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030721     C*** TSP
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
093100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
093200030721     C                   ENDIF
093300030721     C*** NAS
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
093500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
093600030721     C                   ENDIF
093700030721     C*** TIC
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
093900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030721     C*** GCA
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
094300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030721     C*** XCO
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
094700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030721     C*** CTM
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
095100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
095200030721     C                   ENDIF
095300030721     C*** TCR
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
095500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030721     C*** CTS
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
095900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030721     C*** FTM
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
096300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030721     C*** VAD
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
096700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030721     C*** GMA
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
097100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
097200030721     C                   ENDIF
097300030721     C*** GGA
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
097500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030721     C*** GVA
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
097900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030721     C*** TC1
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
098300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** TC2
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
098700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** SCL
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
099100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** RMO
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
099500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** NMO
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
099900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
100000030721     C                   ENDIF
100100040119     C*
100200040119     C* Verifico i campi "personalizzati"
100300040119     C*** 1° parte RMA
100400040119     C*** 1° parte VATNOT
100500040119     C                   IF        %trim(SkSplitFLD(i)) = 'BCALFA'
100600040119     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
100700040119     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
100800040119     C                   ENDIF
100900040119     C*** RMN
101000040119     C*** 2° parte VATNOT
101100040119     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
101200040119     C                   EVAL      PiStr=SkSplitCSV(i)
101300040119     C                   EXSR      CHKNUM
101400040119     C                   IF        PiInt=*on
101500040119     C                   Z-ADD     PiVal         VABRMN
101600040119     C                   Z-ADD     PiVal         VABNSP
101700040119     C                   Z-ADD     PiVal         VATNSP
101800040119     C                   MOVE      VABRMN        wAlfa4
101900040119     C                   EVAL      VABRMA = %trim(VABRMA)+wAlfa4
102000040119     C                   EVAL      VATNOT = %trim(VATNOT)+wAlfa4
102100040119     C                   ELSE
102200040119     C                   SETON                                        32
102300040119     C                   EVAL      VABRMN = 1
102400040119     C                   EVAL      vinmsg = %trimr(vinmsg)
102500040119     C                             + ' ' + 'VABRMN VABNSP VATNSP VATNOT'
102600040119     C                   ENDIF
102700040119     C                   ENDIF
102800040119     C*** 3° parte VATNOT
102900040119     C                   IF        %trim(SkSplitFLD(i)) = 'ANNO'
103000040119     C                   EVAL      VATNOT = %trim(VATNOT)+%trim(SkSplitCSV(i))
103100040119     C                   ENDIF
103200040119     C*** CBO
103300040119     C                   IF        %trim(SkSplitFLD(i)) = 'TIPOPORTO'
103400040119     C                   IF        %trim(SkSplitCSV(i)) = '1'
103500040119     C                   EVAL      VABCBO = '2'
103600040119     C                   ENDIF
103700040119     C                   IF        %trim(SkSplitCSV(i)) = '2'
103800040119     C                   EVAL      VABCBO = '1'
103900040119     C                   ENDIF
104000040119     C                   ENDIF
104100030721     C*
104200030721     C* Reperisco quindi i campi numerici...
104300030721     C*** CAD
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
104500030721     C                   EVAL      PiStr=SkSplitCSV(i)
104600030721     C                   EXSR      CHKNUM
104700030721     C                   IF        PiInt=*on
104800030721     C                   Z-ADD     PiVal         Num5_0
104900030721     C                   MOVEL(p)  Num5_0        VABCAD
105000030721     C                   ELSE
105100030721     C                   SETON                                        32
105200030721     C                   EVAL      VABCAD = *zeros
105300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105400030721     C                             + ' ' + 'VABCAD'
105500030721     C                   ENDIF
105600030721     C                   ENDIF
105700030721     C*** NCL
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
105900030721     C                   EVAL      PiStr=SkSplitCSV(i)
106000030721     C                   EXSR      CHKNUM
106100030721     C                   IF        PiInt=*on
106200030721     C                   Z-ADD     PiVal         VABNCL
106300030721     C                   ELSE
106400030721     C                   SETON                                        32
106500030721     C                   EVAL      VABNCL = *zeros
106600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106700040119     C                             + ' ' + 'VABNCL VATNOT'
106800030721     C                   ENDIF
106900030721     C                   ENDIF
107000030721     C*** PKB
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
107200030721     C                   EVAL      PiStr=SkSplitCSV(i)
107300030721     C                   EXSR      CHKNUM
107400030721     C                   IF        PiNum=*on
107500030721     C                   Z-ADD     PiVal         VABPKB
107600030721     C                   ELSE
107700030721     C                   SETON                                        32
107800030721     C                   EVAL      VABPKB = *zeros
107900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108000030721     C                             + ' ' + 'VABPKB'
108100030721     C                   ENDIF
108200030721     C                   ENDIF
108300030721     C*** CAS
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
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
110400040119     C                   Z-ADD     PiVal         VATCCM
110500030721     C                   ELSE
110600030721     C                   SETON                                        32
110700030721     C                   EVAL      VABCCM = *zeros
110800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110900040119     C                             + ' ' + 'VABCCM VATCCM'
111000030721     C                   ENDIF
111100030721     C                   ENDIF
111200030721     C*** LNP
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiInt=*on    AND
111700030721     C                             PiVal<=999   AND
111800030721     C                             PiVal>*zeros
111900030721     C                   Z-ADD     PiVal         VABLNP
112000040119     C                   Z-ADD     PiVal         VATLNP
112100030721     C                   ELSE
112200030721     C                   SETON                                        32
112300030721     C                   EVAL      VABLNP = *zeros
112400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112500040119     C                             + ' ' + 'VABLNP VATLNP'
112600030721     C                   ENDIF
112700030721     C                   ENDIF
112800030721     C*** NRS
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
113000030721     C                   EVAL      PiStr=SkSplitCSV(i)
113100030721     C                   EXSR      CHKNUM
113200030721     C                   IF        PiInt=*on AND
113300030721     C                             PiVal<=99
113400030721     C                   Z-ADD     PiVal         VABNRS
113500030721     C                   ELSE
113600030721     C                   SETON                                        32
113700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113800030721     C                             + ' ' + 'VABNRS'
113900030721     C                   ENDIF
114000030721     C                   ENDIF
114100030721     C*** LNA
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
114300030721     C                   EVAL      PiStr=SkSplitCSV(i)
114400030721     C                   EXSR      CHKNUM
114500030721     C                   IF        PiInt=*on    AND
114600030721     C                             PiVal<=999   AND
114700030721     C                             PiVal>*zeros
114800030721     C                   Z-ADD     PiVal         VABLNA
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100030721     C                   EVAL      VABLNA = *zeros
115200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115300030721     C                             + ' ' + 'VABLNA'
115400030721     C                   ENDIF
115500030721     C                   ENDIF
115600030721     C*** CTR
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiInt=*on  AND
116100030721     C                             PiVal<=999
116200030721     C                   Z-ADD     PiVal         VABCTR
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600030721     C                             + ' ' + 'VABCTR'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** IAS
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiNum=*on
117400030721     C                   Z-ADD     PiVal         VABIAS
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABIAS'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** VLB
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiNum=*on
118600030721     C                   Z-ADD     PiVal         VABVLB
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119000030721     C                             + ' ' + 'VABVLB'
119100030721     C                   ENDIF
119200030721     C                   ENDIF
119300030721     C*** QFT
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
119500030721     C                   EVAL      PiStr=SkSplitCSV(i)
119600030721     C                   EXSR      CHKNUM
119700030721     C                   IF        PiNum=*on
119800030721     C                   Z-ADD     PiVal         VABQFT
119900030721     C                   ELSE
120000030721     C                   SETON                                        32
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200030721     C                             + ' ' + 'VABQFT'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500030721     C*** NCD
120600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
120700030721     C                   EVAL      PiStr=SkSplitCSV(i)
120800030721     C                   EXSR      CHKNUM
120900030721     C                   IF        PiInt=*on      AND
121000030721     C                             PiVal<=9999999
121100030721     C                   Z-ADD     PiVal         VABNCD
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500030721     C                             + ' ' + 'VABNCD'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** NCA
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiInt=*on      AND
122300030721     C                             PiVal<=9999999
122400030721     C                   Z-ADD     PiVal         VABNCA
122500030721     C                   ELSE
122600030721     C                   SETON                                        32
122700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122800030721     C                             + ' ' + 'VABNCA'
122900030721     C                   ENDIF
123000030721     C                   ENDIF
123100030721     C*** ZNC
123200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
123300030721     C                   EVAL      PiStr=SkSplitCSV(i)
123400030721     C                   EXSR      CHKNUM
123500030721     C                   IF        PiInt=*on AND
123600030721     C                             PiVal<=99
123700030721     C                   Z-ADD     PiVal         VABZNC
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100030721     C                             + ' ' + 'VABZNC'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** DCR
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiInt=*on       AND
124900030721     C                             PiVal<=99999999
125000030721     C                   Z-ADD     PiVal         VABDCR
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABDCR'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** HCR
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on   AND
126200030721     C                             PiVal<=9999
126300030721     C                   Z-ADD     PiVal         VABHCR
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABHCR'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** VMD
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiNum=*on
127500030721     C                   Z-ADD     PiVal         VABVMD
127600030721     C                   ELSE
127700030721     C                   SETON                                        32
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABVMD'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** ANT
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
128400030721     C                   EVAL      PiStr=SkSplitCSV(i)
128500030721     C                   EXSR      CHKNUM
128600030721     C                   IF        PiInt=*on        AND
128700030721     C                             PiVal<=999999999
128800030721     C                   Z-ADD     PiVal         VABANT
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABANT'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** CMO
129600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiInt=*on
130000030721     C                   Z-ADD     PiVal         Num5_0
130100030721     C                   MOVEL(p)  Num5_0        VABCMO
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      VABCMO = *zeros
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABCMO'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030715     C*
131000030715     C                   ENDIF
131100030715     C                   EVAL      i = i + 1
131200030715     C                   ENDDO
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
151700070411
151800070411     C     *pssr         BEGSR
151900070411     C*
152000070411     C                   if        %open(tivin00r)
152100070411     C                   close     tivin00r
152200070411     C                   endif
152300070411     C                   if        %open(fivabwwr)
152400070411     C                   close     fivabwwr
152500070411     C                   endif
152600070411     C                   if        %open(fivatwwr)
152700070411     C                   close     fivatwwr
152800070411     C                   endif
152900070411     C*
153000070411     C* Effettuo la chiamata al CLLE preposto
153100070411     C                   call(e)   'TITVVTC'
153200070411     C                   parm                    parccm
153300070411     C                   parm                    parmbr
153400070411     C                   parm      '2'           paropz
153500070411     C*
153600070411     C                   eval      wrkesito = '2'
153700070411     C*
153800070411     C                   seton                                        LR
153900070411     C*
154000070411     C                   ENDSR     '*CANCL'
154100070411     C***
154200070411
154300050421
154400010330
154500010330
154600000613     C     *inzsr        BEGSR
154700990910     C*
154800990910     C     *entry        plist
154900990920     C                   parm                    tivlrds
155000990921     C                   parm      wrkesito      esito
155100000724     C                   parm                    prmlit
155200000710     C                   parm                    prmfir
155300010330     C*
155400010330     C* CALCOLA LA DATA CORRENTE
155500010330     C                   time                    wn14             14 0
155600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
155700010330     C                   z-add     wn8           g08dat
155800010330     C                   z-add     *zeros        g08inv
155900010330     C                   movel     '0'           g08err
156000010330     C                   call      'XSRDA8'
156100010330     C                   parm                    wlbda8
156200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
156300000613     C*
156400000613     C                   ENDSR
156500000613     C***
156600990908
156700040119     Otitv89p   E            riepilogo         2
156800990915     O                                              'Upload via Internet'
156900990915     O                                           +1 'Traduzione TIVIN00R -'
157000021025     O                                           +1 'FIVABWWR'
157100990915     O                                           +1 'Testate bolle'
157200990915     O          E            riepilogo   2
157300990915     O                       wrkdata
157400990915     O                       wrkora              +1
157500990915     O                       procname            +1
157600990915     O          E            riepilogo   2
157700990915     O                                              'Cliente..................:'
157800010605     O                       VABCCM        z     +1
157900990915     O          E            riepilogo   2
158000990920     O                                              'Riferimento Strategi.....:'
158100990920     O                       vlrhdl              +1
158200990915     O          E            riepilogo   2
158300990915     O                                              'Giusti...................:'
158400040119     O                       §CTROKVB      2   +  1
158500990915     O          E            riepilogo   2
158600990915     O                                              'Sbagliati e corretti.....:'
158700971022     O                       §CTRMO        2   +  1
158800990915     O          E            riepilogo   2
158900990915     O                                              'Sbagliati e scartati.....:'
159000971022     O                       §CTRNO        2   +  1
159100000613
159200040119     Otitv89ps  E            testdett          2
159300000613     O                                              'Upload via Internet'
159400000613     O                                           +1 'Traduzione TIVIN00R -'
159500021025     O                                           +1 'FIVABWWR'
159600000613     O                                           +1 'Testate bolle'
159700000616     O          E            testdett    3
159800000613     O                                           +2 'N° rec'
159900000613     O                                           +3 'Anteprima contenuto'
160000000616     O          E            rigadett    2
160100000613     O                       rrnum               +2
160200000621     O                       recko               +3
160300000616     O          E            findett     2
160400000613     O                       wrkdata
160500000613     O                       wrkora              +1
160600000613     O                       procname            +1
160700000616     O          E            findett     2
160800000613     O                                              'Cliente..................:'
160900010605     O                       VABCCM        z     +1
161000000616     O          E            findett     2
161100000613     O                                              'Riferimento Strategi.....:'
161200000613     O                       vlrhdl              +1
161300000616     O          E            findett     2
161400000613     O                                              'Giusti...................:'
161500040119     O                       §CTROKVB      2   +  1
161600000616     O          E            findett     2
161700000613     O                                              'Sbagliati e corretti.....:'
161800000613     O                       §CTRMO        2   +  1
161900000616     O          E            findett     2
162000000613     O                                              'Sbagliati e scartati.....:'
162100000613     O                       §CTRNO        2   +  1
162200000613** CMD - COMANDI CL
162300040119OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV89P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
162400040119OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV89PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
162500040119DLTOVR FILE(TITV89P TITV89PS) LVL(*)
162600010606
162700000613
162800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
162900040119bcalfa;vabrmn;anno;dataora;vabrsd;vabind;vabcad;vablod;
163000040119vabprd;naturamerce;vabncl;vabpkb;tipoporto;vabcas
163100040113
