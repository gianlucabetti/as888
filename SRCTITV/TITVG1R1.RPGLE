000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400041130     Fazorg01l  IF   e           K DISK
000500041130     Ftivin00r  UF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800041130     Ftitvg1p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100041130     Ftitvg1ps  O    f  198        PRINTER usropn
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
002200041130     D intcol          S            100    DIM(4) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000040706     D tisi95ds      e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040709     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100040119     D parccm          s              8    INZ(*blanks)
004200040119     D parmbr          s             10    INZ(*blanks)
004300040119     D paropz          s              1    INZ(*blanks)
004400040119     D chkcall         s              1    INZ(*blanks)
004500040719     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004600090915     D wBarcode        s                   LIKE(vatNOT) INZ(*blanks)
004700030715     D*------------------
004800041130     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004900030715     D*------------------
005000041130     D SkSplitFLD      S             20    DIM(50)
005100041130     D SkSplitNOT      S             35    DIM(100)
005200041130     D SkSplitCSV      S            600    DIM(50)
005300030715     D CharCSV         S              1
005400030715     D CharTXT         S              1
005500030715     D CharNUM         S              1
005600041130     D i               s              5  0 INZ(1)
005700041130     D y               s              3  0 INZ(1)
005800030715     D wGiro           s              1  0 INZ(*zeros)
005900041130     D wSkParam        s          30000
006000030822     D*------------------
006100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006200030822     D*------------------
006300030822     D posDaDft        S              3  0 INZ(*zeros)
006400030822     D posADft         S              3  0 INZ(*zeros)
006500030822     D j               s              3  0 INZ(1)
006600030822     D wGiroDft        s              1  0 INZ(*zeros)
006700041130     D*------------------
006800090915     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
006900041130     D*------------------
007000041130     D InStringa       S          10240                                         (stringa input)
007100041130     D InSepara        S             10                                         (separatore)
007200041130     D OutArray        S               *                                        (array output)
007300041130     D OutErrore       S              1                                         (flag errore)
007400041130     D OutMsg          S             80                                         (messaggio errore)
007500091008     D SkSplitWRK      S            600    DIM(50)
007600010330     D*------------------
007700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007800010330     D*------------------
007900010330     D WLBDA8          DS                  INZ
008000010330     D  G08DAT                 1      8  0
008100010330     D  G08INV                 9     16  0
008200010330     D  G08ERR                17     17
008300010330     D  G08TGI                18     22  0
008400010330
008500010330
008600010330
008700990915     C                   time                    wrkdata
008800990915     C                   time                    wrkora
008900000913     C                   reset                   rrnum
009000990921     C                   reset                   esito
009100990921     C                   reset                   wrkesito
009200010601     C*
009300010601     C                   exsr      opeini
009400010605     C                   exsr      rwvab
009500010601     C*
009600010601     C                   seton                                        lr
009700010601
009800010601
009900010601
010000010601     C*--------------------------------------------------------
010100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010200010601     C*--------------------------------------------------------
010300010601     C     PREELA        BEGSR
010400010601     C*
010500010601     C* SE OCCORRE SPEDIRE IN FILIALE
010600010601     C                   if        invfil <> *zeros and
010700010601     C                             flgGiro = '0'
010800010601     C*
010900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011000010601     C                   eval      flgGiro = '1'
011100000724     C*
011200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011400010601     C     invfil        chain     azorg01l
011500000724     C                   if        %found
011600000724     C                   movel(p)  CMD(1)        depcmd
011700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011800000724     C*
011900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000724     C                   PARM                    depcmd
012300000724     C                   PARM                    LENGH
012400000724     C*
012500000724     C                   endif
012600000616     C*
012700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012800000616     C                   movel(p)  CMD(2)        depcmd
012900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013000000616     C*
013100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013200011113     C                   Z-ADD     150           LENGH            15 5
013300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013400000616     C                   PARM                    depcmd
013500000616     C                   PARM                    LENGH
013600000613     C*
013700041130     C                   if        not %open(titvg1ps)
013800041130     C                   open      titvg1ps
013900030715     C                   except    testdett
014000000613     C                   endif
014100010601     C*
014200010601     C                   endif
014300010601     C*
014400010601     C                   ENDSR
014500010601     C***
014600010601
014700010601
014800010601
014900010601
015000010601     C*--------------------------------------------------------
015100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015200010601     C*--------------------------------------------------------
015300010601     C     ENDELA        BEGSR
015400990915     C*
015500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015600040119     C*
015700040119     C* Effettuo la chiamata al CLLE preposto
015800040119     C                   call(e)   'TITVVTC'
015900040119     C                   parm                    parccm
016000040119     C                   parm                    parmbr
016100040119     C                   parm      '2'           paropz
016200050201     C*
016300050201     C* Effettuo lancio TISI95 solo x chiusura
016400050201     C                   CLEAR                   TISI95DS
016500050201     C                   EVAL      I95TLA = 'C'
016600050201     C                   CALL      'TISI95R'
016700050201     C                   PARM                    TISI95DS
016800000613     C*
016900041130     C                   if        %open(titvg1ps)
017000000616     C                   except    findett
017100041130     C                   close     titvg1ps
017200000613     C                   endif
017300000616     C*
017400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017500011113     C                   Z-ADD     150           LENGH            15 5
017600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017700000616     C                   PARM                    CMD(3)
017800000616     C                   PARM                    LENGH
017900000616     C*
018000010601     C                   ENDSR
018100010601     C***
018200010601
018300010601
018400010601
018500000613
018600000613     C*--------------------------------------------------------
018700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018800000613     C*--------------------------------------------------------
018900000621     C     STPR          BEGSR
019000000613     C*
019100041130     C                   if        not %open(titvg1p)
019200041130     C   50              open      titvg1p
019300990915     C                   endif
019400990915     C*
019500030715     C   50              except    riepilogo
019600990915     C*
019700041130     C                   if        %open(titvg1p)
019800041130     C   50              close     titvg1p
019900990914     C                   endif
020000000613     C*
020100000613     C                   ENDSR
020200000613     C***
020300000613
020400010601
020500010601
020600010330     C*--------------------------------------------------------
020700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020800010330     C*--------------------------------------------------------
020900010605     C     RWVAB         BEGSR
021000010330     C*
021100010330     C                   if        not %open(tivin00r)
021200010330     C                   open      tivin00r
021300010330     C                   endif
021400021025     C                   if        not %open(fivabwwr)
021500021025     C                   open      fivabwwr
021600010330     C                   endif
021700040119     C*
021800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021900040119     C                   exsr      prevat
022000040119     C*
022100040119     C                   if        chkcall = '0'
022200040119     C*
022300040119     C                   if        not %open(fivatwwr)
022400040119     C                   open      fivatwwr
022500040119     C                   endif
022600010330     C*
022700040119     C                   clear                   §CTROKVB          7 0
022800040119     C                   clear                   §CTROKVT          7 0
022900040119     C                   clear                   §CTRMO            7 0
023000040119     C                   clear                   §CTRNO            7 0
023100040706     C*
023200040706     C  N70              do        *HIVAL
023300010330     C*
023400040706     C                   read      tivin00r                               70
023500010618     C*
023600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023700010618     C                   if        vinflg = '1'
023800010618     C                   eval      flgOk = '1'
023900010618     C                   endif
024000010618     C*
024100040316     C                   if        (vindta > *blanks AND
024200040316     C                              %trim(vindta) <> *all';') OR
024300040316     C                             wGiro = *zeros
024400010330     C                   add       1             rrnum
024500010330     C*
024600010601     C                   if        *in70 = *off and
024700010330     C                             (vinflg = *blanks
024800010330     C                              or vinflg = '0'
024900010330     C                              or vinflg = '2')
025000010330     C*
025100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025200010711     C                   if        vinflg = *blanks or vinflg = '0'
025300010711     C                   clear                   vinmsg
025400010711     C                   endif
025500010601     C*
025600010330     C                   exsr      inzvar
025700090915     C                   exsr      impvab
025800010601     C*
025900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026000010605     C                   eval      depfil = VABLNP
026100010601     C                   exsr      repfil
026200010601     C                   if        depfil = invfil
026300021025     C                   if        vlrpoi = 999
026400030822     C                   move(P)   invfil        VABFGS
026500040119     C                   move(P)   invfil        VATFGS
026600021025     C                   else
026700030822     C                   move(P)   vlrpoi        VABFGS
026800040119     C                   move(P)   vlrpoi        VATFGS
026900021025     C                   endif
027000010601     C*
027100010601     C                   exsr      PREELA
027200010601     C*
027300010604     C* Ebbene...
027400010604     C*
027500040119     C  N31              add       1             §CTROKVB
027600040119     C   32              add       1             §CTRMO
027700040119     C   31              add       1             §CTRNO
027800030822     C                   if        wGiro = 2
027900090915     C*
028000090915     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
028100090915     C                   exsr      wrivat
028200090915     C*
028300040119     C                   if        depspe =  *zeros OR
028400040119     C                             depspe <> vabNSP
028500040119     C  N31              exsr      wrivab
028600040119     C  N31              eval      depspe = vabNSP
028700040119     C                   endif
028800040119     C                   endif
028900010604     C*
029000010604     C                   if        *in31 = *off and
029100010604     C                             *in32 = *off
029200010604     C                   eval      vinflg = '1'
029300010604     C                   else
029400010604     C                   eval      recko = vindta
029500010604     C                   except    rigadett
029600010604     C                   eval      vinflg = '2'
029700010604     C                   endif
029800010604     C                   endif
029900010601     C*
030000010604     C                   endif
030100010604     C*
030200010330     C                   else
030300010330     C                   eval      vinflg = '1'
030400010330     C                   endif
030500010601     C*
030600010601     C  N70              update    tivin000
030700010330     C*
030800030822     C  N70              enddo
030900040119     C*
031000040119     C                   endif                                                  (endif - chkcall)
031100010601     C*
031200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031300010601     C                   if        cntNonEl = *zeros or
031400010601     C                             flgMulti = '0'
031500010330     C* Se non ci sono record con errori ...
031600010601     C                   if        §ctrno = 0 and
031700010604     C                             §ctrmo = 0 and
031800010601     C                             flgStato <> '2'
031900010330     C* ... restituisco esito OK.
032000010330     C                   eval      wrkesito = '0'
032100010330     C                   else
032200040119     C                   if        §ctrokvb > 0
032300010330     C                   eval      wrkesito = '1'
032400010330     C                   else
032500010615     C                   if        flgOk = '0'
032600010615     C                   eval      wrkesito = '2'
032700010615     C                   else
032800010615     C                   eval      wrkesito = '6'
032900010615     C                   endif
033000010330     C                   endif
033100010330     C                   endif
033200010601     C                   else
033300010601     C                   eval      wrkesito = '9'
033400010601     C                   endif
033500010330     C*
033600010330     C                   if        %open(tivin00r)
033700010330     C                   close     tivin00r
033800010330     C                   endif
033900021025     C                   if        %open(fivabwwr)
034000021025     C                   close     fivabwwr
034100010330     C                   endif
034200040512     C                   if        %open(fivatwwr)
034300040512     C                   close     fivatwwr
034400040512     C                   endif
034500010601     C*
034600010601     C                   if        vlrpoi <> 999
034700010601     C                   eval      invfil = vlrpoi
034800010601     C                   endif
034900010330     C*
035000040119     C                   if        §ctrokvb > 0
035100010601     C                             and invfil > *zeros
035200010330     C                   exsr      invio
035300010330     C                   endif
035400010601     C*
035500010618     C                   if        flgGiro = '1'
035600010601     C                   exsr      endela
035700010618     C                   endif
035800010330     C*
035900010330     C                   ENDSR
036000010330     C***
036100040119
036200040119
036300040119
036400040119     C*----------------------------------------------------*
036500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036600040119     C*----------------------------------------------------*
036700040119     C     WRIVAB        BEGSR
036800040119     C*
036900040119     C                   write     fivab000                                     => scarico il VAB
037000040119     C*
037100040119     C                   ENDSR
037200040119
037300040119
037400040119
037500040119     C*----------------------------------------------------*
037600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037700040119     C*----------------------------------------------------*
037800041130     C     WRIVAT        BEGSR
037900040706     C*
038000040719     C* Attribuisco la stessa chiave della testata bolle
038100090915     C                   EVAL      VATAAS = VABAAS
038200040719     C                   EVAL      VATLNP = VABLNP
038300090915     C                   EVAL      VATNRS = VABNRS
038400040719     C                   EVAL      VATNSP = VABNSP
038500090915     C                   EVAL      VATCCM = VABCCM
038600040719     C*
038700041130     C* Imposto il tipo record sul file VAT a 'E' x indicare i "CHI SONO"
038800041130     C                   EVAL      VATTRC = 'E'
038900090915     C*
039000090915     C                   EVAL      VATNOT = wBarcode
039100090915     C                   WRITE     FIVAT000
039200090915     C                   ADD       1             §CTROKVT
039300040119     C*
039400040119     C                   ENDSR
039500040119
039600040119
039700040119
039800040119     C*----------------------------------------------------*
039900040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040000040119     C*----------------------------------------------------*
040100040119     C     PREVAT        BEGSR
040200040119     C*
040300040119     C* Compongo il nome del membro da dare al FIVATWWR
040400040119     C                   eval      parmbr = vlrhdl
040500040119     C                   movel     'M'           parmbr
040600041130     C                   eval      parccm = vlrksc
040700040119     C                   eval      paropz = '1'
040800040119     C* Effettuo la chiamata al CLLE preposto
040900040119     C                   call(e)   'TITVVTC'
041000040119     C                   parm                    parccm
041100040119     C                   parm                    parmbr
041200040119     C                   parm                    paropz
041300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041400040119     C                   if        %error
041500040119     C                   movel     '1'           chkcall
041600040119     C                   else
041700040119     C                   movel     '0'           chkcall
041800040119     C                   endif
041900040119     C*
042000040119     C                   ENDSR
042100010601
042200010601
042300010601
042400010330     C*----------------------------------------------------*
042500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
042600010330     C*----------------------------------------------------*
042700040119     C     INZVAR        BEGSR
042800010330     C*
042900030822     C* Inizializzo variabili di wrk
043000020204     C                   Z-ADD     *zeros        Num5_0            5 0
043100030822     C*
043200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
043300030822     C                   CLEAR                   FIVAB000
043400040119     C                   CLEAR                   FIVAT000
043500030822     C                   CLEAR                   SkSplitCSV
043600041130     C                   CLEAR                   SkSplitNOT
043700030822     C                   EVAL      i = 1
043800030822     C*
043900030822     C* Reimposto i valori di default
044000030822     C                   EXSR      DEFCAM
044100030822     C*
044200010330     C                   ENDSR
044300010330     C*----------------------------------------------------*
044400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
044500010330     C*----------------------------------------------------*
044600010330     C     DEFCAM        BEGSR
044700040119     C*
044800040119     C* Imposto i valori d default a programma dei campi fondamentali
044900041130     C                   EVAL      VABCCM = 0892957
045000041130     C                   EVAL      VATCCM = 0892957
045100041130     C                   EVAL      VABLNP = 089
045200041130     C                   EVAL      VATLNP = 089
045300040706     C                   EVAL      VABCTR = 000
045400040119     C                   EVAL      VABCBO = '1'
045500041130     C                   EVAL      VABTSP = 'C'
045600041130     C                   EVAL      VATTRC = 'E'
045700030822     C*
045800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
045900030715     C* e delimitatore testo.
046000091008     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
046100091008     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
046200091008     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
046300030822     C*
046400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
046500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
046600030715     C                   SETON                                        50
046700030715     C                   ELSE
046800030715     C                   SETOFF                                       50
046900030715     C                   ENDIF
047000030822     C*
047100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
047200030822     C                   EVAL      posDaDft = 1
047300030822     C                   EVAL      posADft  = 0
047400030822     C                   EVAL      wGiroDft = 0
047500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
047600030822     C                             posDaDft > 0
047700030822     C*
047800030822     C* Gestisco il 1° giro
047900030822     C                   IF        wGiroDft = 0
048000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
048100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
048200030822     C* Incremento il contatore dei "giri"
048300030822     C                   EVAL      wGiroDft = 1
048400030822     C                   ELSE
048500030822     C                   EVAL      posDaDft = posADft
048600030822     C                   ENDIF
048700030822     C* Eseguo lo scan x trovare la fine del campo corrente
048800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
048900030822     C*
049000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
049100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
049200030822     C                   IF        posDaDft > 0 AND
049300030822     C                             posADft  > 0
049400030822     C* NCL
049500030822     C                   IF        %subst(
049600030822     C                             %subst(vlrppt:posDaDft+1:
049700030822     C                             posADft-posDaDft-1):1:3)
049800030822     C                             = 'NCL'
049900030822     C                   EVAL      PiStr=%trim(%subst(
050000030822     C                             %subst(vlrppt:posDaDft+1:
050100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050200030822     C                   EXSR      CHKNUM
050300030822     C                   IF        PiInt=*on
050400030822     C                   Z-ADD     PiVal         VABNCL
050500030822     C                   ENDIF
050600030822     C                   ENDIF
050700030822     C* CCM
050800030822     C                   IF        %subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):1:3)
051100030822     C                             = 'CCM'
051200030822     C                   EVAL      PiStr=%trim(%subst(
051300030822     C                             %subst(vlrppt:posDaDft+1:
051400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051500030822     C                   EXSR      CHKNUM
051600030822     C                   IF        PiInt=*on
051700030822     C                   Z-ADD     PiVal         VABCCM
051800040119     C                   Z-ADD     PiVal         VATCCM
051900030822     C                   ENDIF
052000030822     C                   ENDIF
052100030822     C* LNP
052200030822     C                   IF        %subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):1:3)
052500030822     C                             = 'LNP'
052600030822     C                   EVAL      PiStr=%trim(%subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052900030822     C                   EXSR      CHKNUM
053000030822     C                   IF        PiInt=*on
053100030822     C                   Z-ADD     PiVal         VABLNP
053200040119     C                   Z-ADD     PiVal         VATLNP
053300030822     C                   ENDIF
053400030822     C                   ENDIF
053500030822     C* NRS
053600030822     C                   IF        %subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):1:3)
053900030822     C                             = 'NRS'
054000030822     C                   EVAL      PiStr=%trim(%subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300030822     C                   EXSR      CHKNUM
054400030822     C                   IF        PiInt=*on
054500030822     C                   Z-ADD     PiVal         VABNRS
054600030822     C                   ENDIF
054700030822     C                   ENDIF
054800030822     C* CTR
054900030822     C                   IF        %subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):1:3)
055200030822     C                             = 'CTR'
055300030822     C                   EVAL      PiStr=%trim(%subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600030822     C                   EXSR      CHKNUM
055700030822     C                   IF        PiInt=*on
055800030822     C                   Z-ADD     PiVal         VABCTR
055900030822     C                   ENDIF
056000030822     C                   ENDIF
056100030822     C* PKB
056200030822     C                   IF        %subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):1:3)
056500030822     C                             = 'PKB'
056600030822     C                   EVAL      PiStr=%trim(%subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900030822     C                   EXSR      CHKNUM
057000030822     C                   IF        PiNum=*on
057100030822     C                   Z-ADD     PiVal         VABPKB
057200030822     C                   ENDIF
057300030822     C                   ENDIF
057400030822     C* VLB
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'VLB'
057900030822     C                   EVAL      PiStr=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   EXSR      CHKNUM
058300030822     C                   IF        PiNum=*on
058400030822     C                   Z-ADD     PiVal         VABVLB
058500030822     C                   ENDIF
058600030822     C                   ENDIF
058700030822     C* QFT
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'QFT'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiNum=*on
059700030822     C                   Z-ADD     PiVal         VABQFT
059800030822     C                   ENDIF
059900030822     C                   ENDIF
060000030822     C* CBO
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'CBO'
060500030822     C                   EVAL      VABCBO=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   ENDIF
060900030822     C* TSP
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'TSP'
061400030822     C                   EVAL      VABTSP=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   ENDIF
061800030822     C* VAS
061900030822     C                   IF        %subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):1:3)
062200030822     C                             = 'VAS'
062300030822     C                   EVAL      VABVAS=%trim(%subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600030822     C                   ENDIF
062700030822     C* VCA
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'VCA'
063200030822     C                   EVAL      VABVCA=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   ENDIF
063600030822     C* GCA
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'GCA'
064100030822     C                   EVAL      VABGCA=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   ENDIF
064500030822     C* CTM
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'CTM'
065000030822     C                   EVAL      VABCTM=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   ENDIF
065400030822     C* FFD
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'FFD'
065900030822     C                   EVAL      VABFFD=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   ENDIF
066300030822     C* VAD
066400030822     C                   IF        %subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):1:3)
066700030822     C                             = 'VAD'
066800030822     C                   EVAL      VABVAD=%trim(%subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100030822     C                   ENDIF
067200030822     C* GMA
067300030822     C                   IF        %subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):1:3)
067600030822     C                             = 'GMA'
067700030822     C                   EVAL      VABGMA=%trim(%subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068000030822     C                   ENDIF
068100030822     C* GGA
068200030822     C                   IF        %subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):1:3)
068500030822     C                             = 'GGA'
068600030822     C                   EVAL      VABGGA=%trim(%subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900030822     C                   ENDIF
069000030822     C* GVA
069100030822     C                   IF        %subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):1:3)
069400030822     C                             = 'GVA'
069500030822     C                   EVAL      VABGVA=%trim(%subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069800030822     C                   ENDIF
069900030822     C* TC1
070000030822     C                   IF        %subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):1:3)
070300030822     C                             = 'TC1'
070400030822     C                   EVAL      VABTC1=%trim(%subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700030822     C                   ENDIF
070800030822     C* TC2
070900030822     C                   IF        %subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):1:3)
071200030822     C                             = 'TC2'
071300030822     C                   EVAL      VABTC2=%trim(%subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071600030822     C                   ENDIF
071700030822     C* ...
071800030822     C                   ENDIF
071900030822     C                   ENDDO
072000020204     C*
072100010330     C                   ENDSR
072200010607     C*----------------------------------------------------*
072300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
072400010607     C*----------------------------------------------------*
072500010607     C     IMPVAB        BEGSR
072600010607     C*
072700010607     C                   SETOFF                                       3132
072800030515     C*
072900010607     C                   MOVEL     datcor        VABAAS
073000040119     C                   MOVEL     datcor        VATAAS
073100010607     C                   MOVE      datcor        VABMGS
073200041130     C*
073300041130     C* Salvo sempre la riga originale x le eventuali successive sovrapposizioni
073400041130     C                   EVAL      depvinDTA = vinDTA
073500010607     C*
073600030715     C**********
073700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073800030715     C**********
073900030715     C                   IF        wGiro = *zeros
074000040113     C*
074100041130     C* Se necessario inserisco qui la forzatura della intesatazione colonne personalizzata
074200041130     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
074300041130     C                             %trim(intcol(3))+%trim(intcol(4)) <> *blanks
074400041130     C*
074500041130     C* Effettuo la sovrapposizione d intestazioni
074600041130     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
074700041130     C                                      %trim(intcol(3))+%trim(intcol(4))
074800041130     C                   ENDIF
074900041130     C*
075000041130     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
075100041130     C                   CALL      'XSPLIT'
075200041130     C                   PARM      vindta        InStringa
075300041130     C                   PARM      CharCSV       InSepara
075400041130     C*                  PARM                    OutArray
075500041130     C                   PARM                    wSkParam
075600041130     C                   PARM                    OutErrore
075700041130     C                   PARM                    OutMsg
075800041130     C                   MOVEA     wSkParam      SkSplitWRK
075900041130     C*
076000041130     C                   EVAL      i = 1
076100041130     C                   DOW       i <= %elem(SkSplitWRK)
076200041130     C                   EVAL      SkSplitFLD(i) = SkSplitWRK(i)
076300041130     C                   ADD       1             i
076400041130     C                   ENDDO
076500030715     C*
076600041130     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
076700030715     C* delimitazione testo
076800030715     C                   EVAL      i = 1
076900041130     C                   DOW       i <= %elem(SkSplitFLD)
077000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
077100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
077200030715     C                   EVAL      i = i + 1
077300030715     C                   ENDDO
077400030715     C                   EVAL      wGiro = 1
077500030715     C                   ELSE
077600030715     C                   EVAL      wGiro = 2
077700030715     C**********
077800030715     C* Effettuo lo split del campo dati d input
077900030715     C**********
078000041130     C*
078100041130     C* Chiamo il *pgm d utilità x lo "split" della riga dati corrente
078200041130     C                   CALL(e)   'XSPLIT'
078300041130     C                   PARM      vindta        InStringa
078400041130     C                   PARM      CharCSV       InSepara
078500041130     C*                  PARM                    OutArray
078600041130     C                   PARM                    wSkParam
078700041130     C                   PARM                    OutErrore
078800041130     C                   PARM                    OutMsg
078900041130     C                   MOVEA     wSkParam      SkSplitWRK
079000041130     C                   MOVEL     SkSplitWRK    SkSplitCSV
079100030715     C*
079200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079300030715     C* delimitazione testo
079400030715     C                   EVAL      i = 1
079500041130     C                   DOW       i <= %elem(SkSplitCSV)
079600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
079700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
079800030715     C                   EVAL      i = i + 1
079900030715     C                   ENDDO
080000030715     C*
080100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
080200030715     C                   EVAL      i = 1
080300041130     C                   DOW       i <= %elem(SkSplitFLD)
080400030715     C*
080500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
080600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
080700030715     C                             SkSplitCSV(i) <> *blanks
080800030721     C*** ATB
080900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
081000030721     C                   EVAL      VABATB = SkSplitCSV(i)
081100030721     C                   ENDIF
081200030715     C*** RSD
081300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
081400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
081500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
081600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
081700030715     C* ==
081800030715     C                   ENDIF
081900030721     C*** RD2
082000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
082100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
082200030721     C                   ENDIF
082300030715     C*** IND
082400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
082500030715     C                   EVAL      VABIND = SkSplitCSV(i)
082600030715     C                   ENDIF
082700030715     C*** LOD
082800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
082900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
083000030715     C                   ENDIF
083100030715     C*** PRD
083200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
083300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
083400030715     C                   ENDIF
083500030721     C*** NZD
083600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
083700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
083800030721     C                   ENDIF
083900030715     C*** RMA
084000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
084100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
084200030715     C                   ENDIF
084300030715     C*** NOT
084400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
084500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
084600030715     C                   ENDIF
084700030715     C*** NT2
084800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
084900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
085000030715     C                   ENDIF
085100030715     C*** VCA
085200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
085300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
085400030715     C                   ENDIF
085500030715     C*** VAS
085600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
085700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
085800030715     C                   ENDIF
085900030715     C*** FFD
086000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
086100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
086200030715     C                   ENDIF
086300030721     C*** GC1
086400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
086500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
086600030721     C                   ENDIF
086700030721     C*** GC2
086800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
086900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
087000030721     C                   ENDIF
087100030721     C*** TSP
087200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
087300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
087400030721     C                   ENDIF
087500030721     C*** NAS
087600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
087700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
087800030721     C                   ENDIF
087900030721     C*** TIC
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
088100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
088200030721     C                   ENDIF
088300030721     C*** GCA
088400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
088500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
088600030721     C                   ENDIF
088700030721     C*** XCO
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
088900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
089000030721     C                   ENDIF
089100030721     C*** CTM
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
089300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
089400030721     C                   ENDIF
089500030721     C*** TCR
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
089700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030721     C*** CTS
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
090100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** FTM
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
090500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** VAD
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
090900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** GMA
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
091300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030721     C*** GGA
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
091700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030721     C*** GVA
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
092100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** TC1
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
092500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030721     C*** TC2
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
092900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** SCL
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
093300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** RMO
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
093700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*** NMO
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
094100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
094200030721     C                   ENDIF
094300041130     C*
094400040706     C* Reperisco quindi i campi numerici...
094500090915     C*** NSP
094600040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
094700040706     C                   EVAL      PiStr=SkSplitCSV(i)
094800040706     C                   EXSR      CHKNUM
094900041130     C                   IF        PiInt=*on AND PiVal <> *zeros
095000040706     C                   Z-ADD     PiVal         VABNSP
095100040706     C                   ELSE
095200041130     C                   seton                                        31
095300041130     C                   EVAL      VABNSP = *zeros
095400041130     C                   EVAL      vinmsg = %trimr(vinmsg)
095500041130     C                             + ' ' + 'VABNSP VATNSP'
095600040706     C                   ENDIF
095700040706     C                   ENDIF
095800040706     C*** RMN
095900040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
096000040706     C                   EVAL      PiStr=SkSplitCSV(i)
096100040706     C                   EXSR      CHKNUM
096200040706     C                   IF        PiInt=*on
096300040706     C                   Z-ADD     PiVal         VABRMN
096400040706     C                   ELSE
096500040706     C                   SETON                                        32
096600040706     C                   EVAL      VABRMN = 1
096700040706     C                   EVAL      vinmsg = %trimr(vinmsg)
096800040706     C                             + ' ' + 'VABRMN'
096900040706     C                   ENDIF
097000040706     C                   ENDIF
097100030721     C*** CAD
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
097300030721     C                   EVAL      PiStr=SkSplitCSV(i)
097400030721     C                   EXSR      CHKNUM
097500030721     C                   IF        PiInt=*on
097600030721     C                   Z-ADD     PiVal         Num5_0
097700030721     C                   MOVEL(p)  Num5_0        VABCAD
097800030721     C                   ELSE
097900030721     C                   SETON                                        32
098000030721     C                   EVAL      VABCAD = *zeros
098100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098200030721     C                             + ' ' + 'VABCAD'
098300030721     C                   ENDIF
098400030721     C                   ENDIF
098500030721     C*** NCL
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
098700030721     C                   EVAL      PiStr=SkSplitCSV(i)
098800030721     C                   EXSR      CHKNUM
098900030721     C                   IF        PiInt=*on
099000030721     C                   Z-ADD     PiVal         VABNCL
099100030721     C                   ELSE
099200030721     C                   SETON                                        32
099300030721     C                   EVAL      VABNCL = *zeros
099400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099500040706     C                             + ' ' + 'VABNCL'
099600030721     C                   ENDIF
099700030721     C                   ENDIF
099800030721     C*** PKB
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
100000030721     C                   EVAL      PiStr=SkSplitCSV(i)
100100030721     C                   EXSR      CHKNUM
100200030721     C                   IF        PiNum=*on
100300030721     C                   Z-ADD     PiVal         VABPKB
100400030721     C                   ELSE
100500030721     C                   SETON                                        32
100600030721     C                   EVAL      VABPKB = *zeros
100700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100800030721     C                             + ' ' + 'VABPKB'
100900030721     C                   ENDIF
101000030721     C                   ENDIF
101100030721     C*** CAS
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
101300030721     C                   EVAL      PiStr=SkSplitCSV(i)
101400030721     C                   EXSR      CHKNUM
101500030721     C                   IF        PiNum=*on
101600030721     C                   Z-ADD     PiVal         VABCAS
101700030721     C                   ELSE
101800030721     C                   SETON                                        32
101900030721     C                   EVAL      VABCAS = *zeros
102000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102100030721     C                             + ' ' + 'VABCAS'
102200030721     C                   ENDIF
102300030721     C                   ENDIF
102400030721     C*** LNP
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
102600030721     C                   EVAL      PiStr=SkSplitCSV(i)
102700030721     C                   EXSR      CHKNUM
102800030721     C                   IF        PiInt=*on    AND
102900030721     C                             PiVal<=999   AND
103000030721     C                             PiVal>*zeros
103100030721     C                   Z-ADD     PiVal         VABLNP
103200040119     C                   Z-ADD     PiVal         VATLNP
103300030721     C                   ELSE
103400030721     C                   SETON                                        32
103500030721     C                   EVAL      VABLNP = *zeros
103600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103700040119     C                             + ' ' + 'VABLNP VATLNP'
103800030721     C                   ENDIF
103900030721     C                   ENDIF
104000030721     C*** NRS
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
104200030721     C                   EVAL      PiStr=SkSplitCSV(i)
104300030721     C                   EXSR      CHKNUM
104400030721     C                   IF        PiInt=*on AND
104500030721     C                             PiVal<=99
104600030721     C                   Z-ADD     PiVal         VABNRS
104700030721     C                   ELSE
104800030721     C                   SETON                                        32
104900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105000030721     C                             + ' ' + 'VABNRS'
105100030721     C                   ENDIF
105200030721     C                   ENDIF
105300030721     C*** LNA
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
105500030721     C                   EVAL      PiStr=SkSplitCSV(i)
105600030721     C                   EXSR      CHKNUM
105700030721     C                   IF        PiInt=*on    AND
105800030721     C                             PiVal<=999   AND
105900030721     C                             PiVal>*zeros
106000030721     C                   Z-ADD     PiVal         VABLNA
106100030721     C                   ELSE
106200030721     C                   SETON                                        32
106300030721     C                   EVAL      VABLNA = *zeros
106400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106500030721     C                             + ' ' + 'VABLNA'
106600030721     C                   ENDIF
106700030721     C                   ENDIF
106800030721     C*** CTR
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
107000030721     C                   EVAL      PiStr=SkSplitCSV(i)
107100030721     C                   EXSR      CHKNUM
107200030721     C                   IF        PiInt=*on  AND
107300030721     C                             PiVal<=999
107400030721     C                   Z-ADD     PiVal         VABCTR
107500030721     C                   ELSE
107600030721     C                   SETON                                        32
107700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107800030721     C                             + ' ' + 'VABCTR'
107900030721     C                   ENDIF
108000030721     C                   ENDIF
108100030721     C*** IAS
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
108300030721     C                   EVAL      PiStr=SkSplitCSV(i)
108400030721     C                   EXSR      CHKNUM
108500030721     C                   IF        PiNum=*on
108600030721     C                   Z-ADD     PiVal         VABIAS
108700030721     C                   ELSE
108800030721     C                   SETON                                        32
108900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109000030721     C                             + ' ' + 'VABIAS'
109100030721     C                   ENDIF
109200030721     C                   ENDIF
109300030721     C*** VLB
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
109500030721     C                   EVAL      PiStr=SkSplitCSV(i)
109600030721     C                   EXSR      CHKNUM
109700030721     C                   IF        PiNum=*on
109800030721     C                   Z-ADD     PiVal         VABVLB
109900030721     C                   ELSE
110000030721     C                   SETON                                        32
110100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110200030721     C                             + ' ' + 'VABVLB'
110300030721     C                   ENDIF
110400030721     C                   ENDIF
110500030721     C*** QFT
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
110700030721     C                   EVAL      PiStr=SkSplitCSV(i)
110800030721     C                   EXSR      CHKNUM
110900030721     C                   IF        PiNum=*on
111000030721     C                   Z-ADD     PiVal         VABQFT
111100030721     C                   ELSE
111200030721     C                   SETON                                        32
111300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111400030721     C                             + ' ' + 'VABQFT'
111500030721     C                   ENDIF
111600030721     C                   ENDIF
111700030721     C*** NCD
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
111900030721     C                   EVAL      PiStr=SkSplitCSV(i)
112000030721     C                   EXSR      CHKNUM
112100030721     C                   IF        PiInt=*on      AND
112200030721     C                             PiVal<=9999999
112300030721     C                   Z-ADD     PiVal         VABNCD
112400030721     C                   ELSE
112500030721     C                   SETON                                        32
112600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112700030721     C                             + ' ' + 'VABNCD'
112800030721     C                   ENDIF
112900030721     C                   ENDIF
113000030721     C*** NCA
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiInt=*on      AND
113500030721     C                             PiVal<=9999999
113600030721     C                   Z-ADD     PiVal         VABNCA
113700030721     C                   ELSE
113800030721     C                   SETON                                        32
113900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114000030721     C                             + ' ' + 'VABNCA'
114100030721     C                   ENDIF
114200030721     C                   ENDIF
114300030721     C*** ZNC
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
114500030721     C                   EVAL      PiStr=SkSplitCSV(i)
114600030721     C                   EXSR      CHKNUM
114700030721     C                   IF        PiInt=*on AND
114800030721     C                             PiVal<=99
114900030721     C                   Z-ADD     PiVal         VABZNC
115000030721     C                   ELSE
115100030721     C                   SETON                                        32
115200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115300030721     C                             + ' ' + 'VABZNC'
115400030721     C                   ENDIF
115500030721     C                   ENDIF
115600030721     C*** DCR
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiInt=*on       AND
116100030721     C                             PiVal<=99999999
116200030721     C                   Z-ADD     PiVal         VABDCR
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600030721     C                             + ' ' + 'VABDCR'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** HCR
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiInt=*on   AND
117400030721     C                             PiVal<=9999
117500030721     C                   Z-ADD     PiVal         VABHCR
117600030721     C                   ELSE
117700030721     C                   SETON                                        32
117800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117900030721     C                             + ' ' + 'VABHCR'
118000030721     C                   ENDIF
118100030721     C                   ENDIF
118200030721     C*** VMD
118300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
118400030721     C                   EVAL      PiStr=SkSplitCSV(i)
118500030721     C                   EXSR      CHKNUM
118600030721     C                   IF        PiNum=*on
118700030721     C                   Z-ADD     PiVal         VABVMD
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABVMD'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** ANT
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiInt=*on        AND
119900030721     C                             PiVal<=999999999
120000030721     C                   Z-ADD     PiVal         VABANT
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400030721     C                             + ' ' + 'VABANT'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** CMO
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiInt=*on
121200030721     C                   Z-ADD     PiVal         Num5_0
121300030721     C                   MOVEL(p)  Num5_0        VABCMO
121400030721     C                   ELSE
121500030721     C                   SETON                                        32
121600030721     C                   EVAL      VABCMO = *zeros
121700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121800030721     C                             + ' ' + 'VABCMO'
121900030721     C                   ENDIF
122000030721     C                   ENDIF
122100040706     C*
122200090915     C*** W_VABNSP
122300090915     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
122400090915     C                   EVAL      PiStr=SkSplitCSV(i)
122500090915     C                   EXSR      CHKNUM
122600090915     C                   IF        PiInt=*on AND PiVal <> *zeros
122700090915     C                   Z-ADD     PiVal         VABNSP
122800090915     C                   Z-ADD     PiVal         VABRMN
122900090915     C                   ELSE
123000090915     C                   seton                                        31
123100090915     C                   EVAL      VABNSP = *zeros
123200090915     C                   EVAL      vinmsg = %trimr(vinmsg)
123300090915     C                             + ' ' + 'VABNSP VATNSP VABRMN'
123400090915     C                   ENDIF
123500090915     C                   ENDIF
123600041130     C*** VATNOT
123700041130     C                   IF        %trim(SkSplitFLD(i)) = 'BARCODES'
123800090915     C                   EVAL      wBarcode=SkSplitCSV(i)
123900041130     C                   ENDIF
124000030715     C*
124100030715     C                   ENDIF
124200030715     C                   EVAL      i = i + 1
124300030715     C                   ENDDO
124400040706     C*
124500040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
124600040706     C* Reperisco la provincia dal CAP e dalla località
124700040706     C                   IF        VABCAD <> *blanks
124800040706     C                   CLEAR                   TISI95DS
124900041130     C                   EVAL      I95TCN = '4'
125000040706     C                   Z-ADD     datcor        I95DAT
125100040706     C                   EVAL      I95CAP = VABCAD
125200040706     C                   EVAL      I95LOC = VABLOD
125300040706     C                   CALL      'TISI95R'
125400040706     C                   PARM                    TISI95DS
125500040706     C                   EVAL      VABPRD = O95PRV
125600040706     C                   ENDIF
125700040719     C*
125800040719     C* Se NSP nn valorizzato considero errore bloccante in traduzione
125900040719     C                   IF        VABNSP = *zeros
126000040719     C                   SETON                                        31
126100041130     C                   EVAL      vinmsg = %trimr(vinmsg)
126200041130     C                             + ' ' + 'VABNSP VATNSP'
126300040719     C                   ENDIF
126400040719     C*
126500040719     C* Se LNP nn valorizzato considero errore bloccante in traduzione
126600040719     C                   IF        VABLNP = *zeros
126700040719     C                   SETON                                        31
126800041130     C                   EVAL      vinmsg = %trimr(vinmsg)
126900041130     C                             + ' ' + 'VABLNP'
127000040719     C                   ENDIF
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
128300040119     C                   EVAL      VABVCA = 'EUR'
128400040119     C                   IF        VABCBO = '1'
128500040113     C                   EVAL      VABCBO = '4'
128600040119     C                   ENDIF
128700040119     C                   IF        VABCBO = '2'
128800040119     C                   EVAL      VABCBO = '6'
128900040119     C                   ENDIF
129000040113     C                   ENDIF
129100020204     C*
129200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
129300020204     C                   EXSR      CHKIMPDIV
129400030715     C                   ENDIF
129500041130     C*
129600041130     C* Ripristino la riga originale
129700041130     C                   EVAL      vinDTA = depvinDTA
129800020204     C*
129900010607     C                   ENDSR
130000010607     C*----------------------------------------------------*
130100010601
130200020204
130300020204     C*----------------------------------------------------*
130400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
130500020204     C*----------------------------------------------------*
130600020204     C     CHKIMPDIV     BEGSR
130700020204     C*
130800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
130900020204     C                   Z-ADD     *zeros        wrkDec            9 9
131000020204     C*
131100020204     C* Come prima cosa effettuo considerazioni sulla divisa
131200020204     C                   IF        vabIAS > *zeros
131300020204     C                   IF        vabVAS <> 'EUR'
131400020204     C                   EVAL      vabVAS =  'ITL'
131500020204     C                   ENDIF
131600020204     C                   ENDIF
131700020204     C*
131800020204     C                   IF        vabCAS > *zeros
131900020204     C                   IF        vabVCA <> 'EUR'
132000020204     C                   EVAL      vabVCA =  'ITL'
132100020204     C                   ENDIF
132200020204     C                   ENDIF
132300020204     C*
132400020204     C                   IF        vabVMD > *zeros
132500020204     C                   IF        vabVAD <> 'EUR'
132600020204     C                   EVAL      vabVAD =  'ITL'
132700020204     C                   ENDIF
132800020204     C                   ENDIF
132900020204     C*
133000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
133100020204     C                   Z-ADD     vabIAS        wrkDec
133200020204     C                   IF        wrkDec > *zeros
133300020204     C                   IF        vabVAS = 'ITL'
133400020204     C                   EVAL      vabIAS = *zeros
133500020204     C                   ENDIF
133600020204     C                   ENDIF
133700020204     C*
133800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
133900020204     C                   Z-ADD     vabCAS        wrkDec
134000020204     C                   IF        wrkDec > *zeros
134100020204     C                   IF        vabVCA = 'ITL'
134200020204     C                   EVAL      vabCAS = *zeros
134300020204     C                   ENDIF
134400020204     C                   ENDIF
134500020204     C*
134600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
134700020204     C                   Z-ADD     vabVMD        wrkDec
134800020204     C                   IF        wrkDec > *zeros
134900020204     C                   IF        vabVAD = 'ITL'
135000020204     C                   EVAL      vabVMD = *zeros
135100020204     C                   ENDIF
135200020204     C                   ENDIF
135300020204     C*
135400020204     C                   ENDSR
135500020204     C***
135600020204
135700010330
135800010330
135900010330     C*----------------------------------------------------*
136000010330     C*  CONTROLLO NUMERICITA' CAMPI
136100010330     C*----------------------------------------------------*
136200010330     C     CHKNUM        BEGSR
136300010330     C*
136400010606     C                   IF        PiDecChr = *blanks
136500030715     C                   EVAL      PiDecChr = CharNUM
136600010606     C                   ENDIF
136700010606     C*
136800010606     C                   CALL(e)   'ISNUMERIC'
136900010330     C                   PARM                    PiStr            30
137000010606     C                   PARM                    PiDecChr          1
137100010330     C                   PARM      *ZEROS        PiVal            30 9
137200010330     C                   PARM      '0'           PiInt             1
137300010330     C                   PARM      '0'           PiNum             1
137400010330     C                   IF        %error
137500010606     C                   EVAL      PiNum=*off
137600010330     C                   ENDIF
137700010330     C*
137800010330     C                   ENDSR
137900010330     C***
138000010330
138100010601
138200010601
138300010601
138400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
138500010601     C     repfil        BEGSR
138600010601     C*
138700010601     C                   if        invfil = *zeros and
138800010601     C                             depfil > *zeros and
138900010629     C                             (vinflg = *blanks or
139000010629     C                              vinflg = *zeros)
139100010601     C
139200010601     C                   eval      invfil = depfil
139300010601     C                   endif
139400010601     C*
139500010601     C                   if        depfil <> invfil and
139600010601     C                             invfil > *zeros
139700010601     C                   eval      flgMulti = '1'
139800010601     C                   if        vinflg = *blanks
139900010601     C                   add       1             cntNonEl
140000010601     C                   endif
140100010601     C                   endif
140200010601     C*
140300010601     C                   if        vinflg = '2'
140400010601     C                   eval      flgStato = '2'
140500010601     C                   endif
140600010601     C*
140700010601     C                   ENDSR
140800010601     C***
140900010601
141000010601
141100010601
141200010330
141300040119      /TITLE Invio dei dati al punto operativo.
141400040119     C     invio         BEGSR
141500040119     C*
141600040119     C* 1° invio FIVAT
141700040119     C                   reset                   dscmz
141800040119     C                   move      vlrpoi        cmzdst
141900040119     C                   eval      cmzfld = 'FIVATWWR'
142000040119     C                   eval      cmzmbd = vlrhdl
142100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
142200040119     C***                if        prmfir = *blanks
142300040119     C                   eval      cmzfla = 'FIVAT00F'
142400040119     C                   eval      cmzmba = 'FIVAT00F'
142500040119     C***                else
142600040119     C***                eval      cmzfla = prmfir
142700040119     C***                eval      cmzmba = prmfir
142800040119     C***                endif
142900040119     C                   eval      cmznrr = *zeros
143000040119     C                   move      §ctrokvt      cmznrr
143100040119     C                   eval      cmzlba = vlrfl1
143200040119     C                   call(e)   'TIS711C'
143300040119     C                   parm                    dscmz
143400040119     C                   parm      *blanks       esito
143500040119     C                   if        %error
143600040119     C                             or cmzerr = '1'
143700040119     C                             or esito  = '1'
143800040119     C                   eval      wrkesito = '3'
143900040119     C                   else
144000040119     C*
144100040119     C* 2° invio FIVAB
144200040119     C                   reset                   dscmz
144300040119     C                   move      vlrpoi        cmzdst
144400040119     C                   eval      cmzfld = vlrfou
144500040119     C                   eval      cmzmbd = vlrhdl
144600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
144700040119     C***                if        prmfir = *blanks
144800040119     C                   eval      cmzfla = 'FIVAB00F'
144900040119     C                   eval      cmzmba = 'FIVAB00F'
145000040119     C***                else
145100040119     C***                eval      cmzfla = prmfir
145200040119     C***                eval      cmzmba = prmfir
145300040119     C***                endif
145400040119     C                   eval      cmznrr = *zeros
145500040119     C                   move      §ctrokvb      cmznrr
145600040119     C                   eval      cmzlba = vlrfl1
145700040119     C                   call(e)   'TIS711C'
145800040119     C                   parm                    dscmz
145900040119     C                   parm      *blanks       esito
146000040119     C                   if        %error
146100040119     C                             or cmzerr = '1'
146200040119     C                             or esito  = '1'
146300040119     C                   eval      wrkesito = '3'
146400040119     C                   endif
146500040119     C                   endif
146600040119     C*
146700040119     C                   ENDSR
146800040119     C***
146900010601
147000010601
147100010601
147200010601
147300010601      /TITLE Invio dei dati al punto operativo.
147400010601     C     opeini        BEGSR
147500010601     C*
147600010601     C* Inizializzo flag e contatori operativi
147700010601     C                   movel     '0'           flgGiro           1
147800010601     C                   movel     '0'           flgMulti          1
147900010601     C                   movel     '1'           flgStato          1
148000010615     C                   movel     '0'           flgOk             1
148100010601     C                   z-add     *zeros        cntNonEl         10 0
148200010601     C                   z-add     *zeros        depfil            3 0
148300010601     C                   z-add     *zeros        invfil            3 0
148400010601     C*
148500010601     C                   ENDSR
148600010601     C***
148700010601
148800010601
148900010601
149000070411
149100070411     C     *pssr         BEGSR
149200070411     C*
149300070411     C                   if        %open(tivin00r)
149400070411     C                   close     tivin00r
149500070411     C                   endif
149600070411     C                   if        %open(fivabwwr)
149700070411     C                   close     fivabwwr
149800070411     C                   endif
149900070411     C                   if        %open(fivatwwr)
150000070411     C                   close     fivatwwr
150100070411     C                   endif
150200070411     C*
150300070411     C* Effettuo la chiamata al CLLE preposto
150400070411     C                   call(e)   'TITVVTC'
150500070411     C                   parm                    parccm
150600070411     C                   parm                    parmbr
150700070411     C                   parm      '2'           paropz
150800070411     C*
150900070411     C                   eval      wrkesito = '2'
151000070411     C*
151100070411     C                   seton                                        LR
151200070411     C*
151300070411     C                   ENDSR     '*CANCL'
151400070411     C***
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
153700990908
153800041130     Otitvg1p   E            riepilogo         2
153900990915     O                                              'Upload via Internet'
154000990915     O                                           +1 'Traduzione TIVIN00R -'
154100021025     O                                           +1 'FIVABWWR'
154200990915     O                                           +1 'Testate bolle'
154300990915     O          E            riepilogo   2
154400990915     O                       wrkdata
154500990915     O                       wrkora              +1
154600990915     O                       procname            +1
154700990915     O          E            riepilogo   2
154800990915     O                                              'Cliente..................:'
154900010605     O                       VABCCM        z     +1
155000990915     O          E            riepilogo   2
155100990920     O                                              'Riferimento Strategi.....:'
155200990920     O                       vlrhdl              +1
155300990915     O          E            riepilogo   2
155400990915     O                                              'Giusti...................:'
155500040119     O                       §CTROKVB      2   +  1
155600990915     O          E            riepilogo   2
155700990915     O                                              'Sbagliati e corretti.....:'
155800971022     O                       §CTRMO        2   +  1
155900990915     O          E            riepilogo   2
156000990915     O                                              'Sbagliati e scartati.....:'
156100971022     O                       §CTRNO        2   +  1
156200000613
156300041130     Otitvg1ps  E            testdett          2
156400000613     O                                              'Upload via Internet'
156500000613     O                                           +1 'Traduzione TIVIN00R -'
156600021025     O                                           +1 'FIVABWWR'
156700000613     O                                           +1 'Testate bolle'
156800000616     O          E            testdett    3
156900000613     O                                           +2 'N° rec'
157000000613     O                                           +3 'Anteprima contenuto'
157100000616     O          E            rigadett    2
157200000613     O                       rrnum               +2
157300000621     O                       recko               +3
157400000616     O          E            findett     2
157500000613     O                       wrkdata
157600000613     O                       wrkora              +1
157700000613     O                       procname            +1
157800000616     O          E            findett     2
157900000613     O                                              'Cliente..................:'
158000010605     O                       VABCCM        z     +1
158100000616     O          E            findett     2
158200000613     O                                              'Riferimento Strategi.....:'
158300000613     O                       vlrhdl              +1
158400000616     O          E            findett     2
158500000613     O                                              'Giusti...................:'
158600040119     O                       §CTROKVB      2   +  1
158700000616     O          E            findett     2
158800000613     O                                              'Sbagliati e corretti.....:'
158900000613     O                       §CTRMO        2   +  1
159000000616     O          E            findett     2
159100000613     O                                              'Sbagliati e scartati.....:'
159200000613     O                       §CTRNO        2   +  1
159300000613** CMD - COMANDI CL
159400041130OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVG1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
159500041130OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVG1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
159600041130DLTOVR FILE(TITVG1P TITVG1PS) LVL(*)
159700010606
159800000613
159900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
160000090915FILL01;FILL02;FILL03;W_VABNSP;VABNCL;FILL05;VABPKB;VABRSD;VABRD2;
160100091008VABIND;FILL06;VABCAD;VABLOD;BARCODES;FILL07;FILL08
160200091008
160300090915
