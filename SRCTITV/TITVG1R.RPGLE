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
004600041130     D*------------------
004700041130     D* DS REPERIMENTO NUMERATORE
004800041130     D*------------------
004900041130     D trul33ds      e ds                  inz
005000041130     D*------------------
005100041130     D* DS ARCHITETTURA
005200041130     D*------------------
005300041130     D kpjba         e ds                  inz
005400030715     D*------------------
005500041130     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005600030715     D*------------------
005700041130     D SkSplitFLD      S             20    DIM(50)
005800041130     D SkSplitNOT      S             35    DIM(100)
005900041130     D SkSplitCSV      S            600    DIM(50)
006000030715     D CharCSV         S              1
006100030715     D CharTXT         S              1
006200030715     D CharNUM         S              1
006300041130     D i               s              5  0 INZ(1)
006400041130     D y               s              3  0 INZ(1)
006500030715     D wGiro           s              1  0 INZ(*zeros)
006600041130     D wOldSep         s              3    INZ(*blanks)
006700041130     D wNewSep         s              3    INZ(*blanks)
006800041130     D wSkParam        s          30000
006900030822     D*------------------
007000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007100030822     D*------------------
007200030822     D posDaDft        S              3  0 INZ(*zeros)
007300030822     D posADft         S              3  0 INZ(*zeros)
007400030822     D j               s              3  0 INZ(1)
007500030822     D wGiroDft        s              1  0 INZ(*zeros)
007600041130     D*------------------
007700041130     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
007800041130     D*------------------
007900041130     D InStringa       S          10240                                         (stringa input)
008000041130     D InSepara        S             10                                         (separatore)
008100041130     D OutArray        S               *                                        (array output)
008200041130     D OutErrore       S              1                                         (flag errore)
008300041130     D OutMsg          S             80                                         (messaggio errore)
008400041130     D SkSplitWRK      S            600    DIM(50)                              *XXX MOMENTANEO
008500041130     D*SkSplitWRK      S           1024    DIM(2048)
008600041130     D*                                    BASED(OutArray)
008700010330     D*------------------
008800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
008900010330     D*------------------
009000010330     D WLBDA8          DS                  INZ
009100010330     D  G08DAT                 1      8  0
009200010330     D  G08INV                 9     16  0
009300010330     D  G08ERR                17     17
009400010330     D  G08TGI                18     22  0
009500010330
009600010330
009700010330
009800990915     C                   time                    wrkdata
009900990915     C                   time                    wrkora
010000000913     C                   reset                   rrnum
010100990921     C                   reset                   esito
010200990921     C                   reset                   wrkesito
010300010601     C*
010400010601     C                   exsr      opeini
010500010605     C                   exsr      rwvab
010600010601     C*
010700010601     C                   seton                                        lr
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011300010601     C*--------------------------------------------------------
011400010601     C     PREELA        BEGSR
011500010601     C*
011600010601     C* SE OCCORRE SPEDIRE IN FILIALE
011700010601     C                   if        invfil <> *zeros and
011800010601     C                             flgGiro = '0'
011900010601     C*
012000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012100010601     C                   eval      flgGiro = '1'
012200000724     C*
012300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012500010601     C     invfil        chain     azorg01l
012600000724     C                   if        %found
012700000724     C                   movel(p)  CMD(1)        depcmd
012800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012900000724     C*
013000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013100011113     C                   Z-ADD     150           LENGH            15 5
013200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013300000724     C                   PARM                    depcmd
013400000724     C                   PARM                    LENGH
013500000724     C*
013600000724     C                   endif
013700000616     C*
013800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013900000616     C                   movel(p)  CMD(2)        depcmd
014000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014100000616     C*
014200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014300011113     C                   Z-ADD     150           LENGH            15 5
014400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014500000616     C                   PARM                    depcmd
014600000616     C                   PARM                    LENGH
014700000613     C*
014800041130     C                   if        not %open(titvg1ps)
014900041130     C                   open      titvg1ps
015000030715     C                   except    testdett
015100000613     C                   endif
015200010601     C*
015300010601     C                   endif
015400010601     C*
015500010601     C                   ENDSR
015600010601     C***
015700010601
015800010601
015900010601
016000010601
016100010601     C*--------------------------------------------------------
016200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016300010601     C*--------------------------------------------------------
016400010601     C     ENDELA        BEGSR
016500990915     C*
016600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016700040119     C*
016800040119     C* Effettuo la chiamata al CLLE preposto
016900040119     C                   call(e)   'TITVVTC'
017000040119     C                   parm                    parccm
017100040119     C                   parm                    parmbr
017200040119     C                   parm      '2'           paropz
017300050201     C*
017400050201     C* Effettuo lancio TISI95 solo x chiusura
017500050201     C                   CLEAR                   TISI95DS
017600050201     C                   EVAL      I95TLA = 'C'
017700050201     C                   CALL      'TISI95R'
017800050201     C                   PARM                    TISI95DS
017900000613     C*
018000041130     C                   if        %open(titvg1ps)
018100000616     C                   except    findett
018200041130     C                   close     titvg1ps
018300000613     C                   endif
018400000616     C*
018500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018600011113     C                   Z-ADD     150           LENGH            15 5
018700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018800000616     C                   PARM                    CMD(3)
018900000616     C                   PARM                    LENGH
019000000616     C*
019100010601     C                   ENDSR
019200010601     C***
019300010601
019400010601
019500010601
019600000613
019700000613     C*--------------------------------------------------------
019800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019900000613     C*--------------------------------------------------------
020000000621     C     STPR          BEGSR
020100000613     C*
020200041130     C                   if        not %open(titvg1p)
020300041130     C   50              open      titvg1p
020400990915     C                   endif
020500990915     C*
020600030715     C   50              except    riepilogo
020700990915     C*
020800041130     C                   if        %open(titvg1p)
020900041130     C   50              close     titvg1p
021000990914     C                   endif
021100000613     C*
021200000613     C                   ENDSR
021300000613     C***
021400000613
021500010601
021600010601
021700010330     C*--------------------------------------------------------
021800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
021900010330     C*--------------------------------------------------------
022000010605     C     RWVAB         BEGSR
022100010330     C*
022200010330     C                   if        not %open(tivin00r)
022300010330     C                   open      tivin00r
022400010330     C                   endif
022500021025     C                   if        not %open(fivabwwr)
022600021025     C                   open      fivabwwr
022700010330     C                   endif
022800040119     C*
022900040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023000040119     C                   exsr      prevat
023100040119     C*
023200040119     C                   if        chkcall = '0'
023300040119     C*
023400040119     C                   if        not %open(fivatwwr)
023500040119     C                   open      fivatwwr
023600040119     C                   endif
023700010330     C*
023800040119     C                   clear                   §CTROKVB          7 0
023900040119     C                   clear                   §CTROKVT          7 0
024000040119     C                   clear                   §CTRMO            7 0
024100040119     C                   clear                   §CTRNO            7 0
024200040706     C*
024300040706     C  N70              do        *HIVAL
024400010330     C*
024500040706     C                   read      tivin00r                               70
024600010618     C*
024700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
024800010618     C                   if        vinflg = '1'
024900010618     C                   eval      flgOk = '1'
025000010618     C                   endif
025100010618     C*
025200040316     C                   if        (vindta > *blanks AND
025300040316     C                              %trim(vindta) <> *all';') OR
025400040316     C                             wGiro = *zeros
025500010330     C                   add       1             rrnum
025600010330     C*
025700010601     C                   if        *in70 = *off and
025800010330     C                             (vinflg = *blanks
025900010330     C                              or vinflg = '0'
026000010330     C                              or vinflg = '2')
026100010330     C*
026200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026300010711     C                   if        vinflg = *blanks or vinflg = '0'
026400010711     C                   clear                   vinmsg
026500010711     C                   endif
026600010601     C*
026700010330     C                   exsr      inzvar
026800010605     C                   exsr      impvab
026900010601     C*
027000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027100010605     C                   eval      depfil = VABLNP
027200010601     C                   exsr      repfil
027300010601     C                   if        depfil = invfil
027400021025     C                   if        vlrpoi = 999
027500030822     C                   move(P)   invfil        VABFGS
027600040119     C                   move(P)   invfil        VATFGS
027700021025     C                   else
027800030822     C                   move(P)   vlrpoi        VABFGS
027900040119     C                   move(P)   vlrpoi        VATFGS
028000021025     C                   endif
028100010601     C*
028200010601     C                   exsr      PREELA
028300010601     C*
028400010604     C* Ebbene...
028500010604     C*
028600040119     C  N31              add       1             §CTROKVB
028700040119     C   32              add       1             §CTRMO
028800040119     C   31              add       1             §CTRNO
028900030822     C                   if        wGiro = 2
029000040119     C                   if        depspe =  *zeros OR
029100040119     C                             depspe <> vabNSP
029200040119     C  N31              exsr      wrivab
029300040119     C  N31              eval      depspe = vabNSP
029400040119     C                   endif
029500040119     C                   endif
029600010604     C*
029700010604     C                   if        *in31 = *off and
029800010604     C                             *in32 = *off
029900010604     C                   eval      vinflg = '1'
030000010604     C                   else
030100010604     C                   eval      recko = vindta
030200010604     C                   except    rigadett
030300010604     C                   eval      vinflg = '2'
030400010604     C                   endif
030500010604     C                   endif
030600010601     C*
030700010604     C                   endif
030800010604     C*
030900010330     C                   else
031000010330     C                   eval      vinflg = '1'
031100010330     C                   endif
031200010601     C*
031300010601     C  N70              update    tivin000
031400010330     C*
031500030822     C  N70              enddo
031600040119     C*
031700040119     C                   endif                                                  (endif - chkcall)
031800010601     C*
031900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
032000010601     C                   if        cntNonEl = *zeros or
032100010601     C                             flgMulti = '0'
032200010330     C* Se non ci sono record con errori ...
032300010601     C                   if        §ctrno = 0 and
032400010604     C                             §ctrmo = 0 and
032500010601     C                             flgStato <> '2'
032600010330     C* ... restituisco esito OK.
032700010330     C                   eval      wrkesito = '0'
032800010330     C                   else
032900040119     C                   if        §ctrokvb > 0
033000010330     C                   eval      wrkesito = '1'
033100010330     C                   else
033200010615     C                   if        flgOk = '0'
033300010615     C                   eval      wrkesito = '2'
033400010615     C                   else
033500010615     C                   eval      wrkesito = '6'
033600010615     C                   endif
033700010330     C                   endif
033800010330     C                   endif
033900010601     C                   else
034000010601     C                   eval      wrkesito = '9'
034100010601     C                   endif
034200010330     C*
034300010330     C                   if        %open(tivin00r)
034400010330     C                   close     tivin00r
034500010330     C                   endif
034600021025     C                   if        %open(fivabwwr)
034700021025     C                   close     fivabwwr
034800010330     C                   endif
034900040512     C                   if        %open(fivatwwr)
035000040512     C                   close     fivatwwr
035100040512     C                   endif
035200010601     C*
035300010601     C                   if        vlrpoi <> 999
035400010601     C                   eval      invfil = vlrpoi
035500010601     C                   endif
035600010330     C*
035700040119     C                   if        §ctrokvb > 0
035800010601     C                             and invfil > *zeros
035900010330     C                   exsr      invio
036000010330     C                   endif
036100010601     C*
036200010618     C                   if        flgGiro = '1'
036300010601     C                   exsr      endela
036400010618     C                   endif
036500010330     C*
036600010330     C                   ENDSR
036700010330     C***
036800040119
036900040119
037000040119
037100040119     C*----------------------------------------------------*
037200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037300040119     C*----------------------------------------------------*
037400040119     C     WRIVAB        BEGSR
037500040119     C*
037600040119     C                   write     fivab000                                     => scarico il VAB
037700040119     C*
037800040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
037900040119     C                   exsr      wrivat
038000040119     C*
038100040119     C                   ENDSR
038200040119
038300040119
038400040119
038500040119     C*----------------------------------------------------*
038600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038700040119     C*----------------------------------------------------*
038800041130     C     WRIVAT        BEGSR
038900040706     C*
039000040719     C* Attribuisco la stessa chiave della testata bolle
039100040719     C                   EVAL      VATAAS = VABAAS
039200040719     C                   EVAL      VATLNP = VABLNP
039300040719     C                   EVAL      VATNSP = VABNSP
039400040719     C*
039500041130     C* Imposto il tipo record sul file VAT a 'E' x indicare i "CHI SONO"
039600041130     C                   EVAL      VATTRC = 'E'
039700041130     C*
039800041130     C* SCarico il buffer del FIVAT x ogni "CHI SONO" della spedizione corrente (da schiera)
039900041130     C                   EVAL      y = 1
040000041130     C                   DOW       y <= %elem(SkSplitWRK)
040100041130     C                   IF        SkSplitNOT(y) <> *blanks
040200041130     C                   EVAL      VATNOT = SkSplitNOT(y)
040300041130     C                   WRITE     FIVAT000
040400041130     C                   ADD       1             §CTROKVT
040500041130     C                   ENDIF
040600041130     C                   ADD       1             y
040700041130     C                   ENDDO
040800040119     C*
040900040119     C                   ENDSR
041000040119
041100040119
041200040119
041300040119     C*----------------------------------------------------*
041400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041500040119     C*----------------------------------------------------*
041600040119     C     PREVAT        BEGSR
041700040119     C*
041800040119     C* Compongo il nome del membro da dare al FIVATWWR
041900040119     C                   eval      parmbr = vlrhdl
042000040119     C                   movel     'M'           parmbr
042100041130     C                   eval      parccm = vlrksc
042200040119     C                   eval      paropz = '1'
042300040119     C* Effettuo la chiamata al CLLE preposto
042400040119     C                   call(e)   'TITVVTC'
042500040119     C                   parm                    parccm
042600040119     C                   parm                    parmbr
042700040119     C                   parm                    paropz
042800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042900040119     C                   if        %error
043000040119     C                   movel     '1'           chkcall
043100040119     C                   else
043200040119     C                   movel     '0'           chkcall
043300040119     C                   endif
043400040119     C*
043500040119     C                   ENDSR
043600010601
043700010601
043800010601
043900010330     C*----------------------------------------------------*
044000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
044100010330     C*----------------------------------------------------*
044200040119     C     INZVAR        BEGSR
044300010330     C*
044400030822     C* Inizializzo variabili di wrk
044500020204     C                   Z-ADD     *zeros        Num5_0            5 0
044600030822     C*
044700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
044800030822     C                   CLEAR                   FIVAB000
044900040119     C                   CLEAR                   FIVAT000
045000030822     C                   CLEAR                   SkSplitCSV
045100041130     C                   CLEAR                   SkSplitNOT
045200030822     C                   EVAL      i = 1
045300030822     C*
045400030822     C* Reimposto i valori di default
045500030822     C                   EXSR      DEFCAM
045600030822     C*
045700010330     C                   ENDSR
045800010330     C*----------------------------------------------------*
045900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
046000010330     C*----------------------------------------------------*
046100010330     C     DEFCAM        BEGSR
046200040119     C*
046300040119     C* Imposto i valori d default a programma dei campi fondamentali
046400041130     C                   EVAL      VABCCM = 0892957
046500041130     C                   EVAL      VATCCM = 0892957
046600041130     C                   EVAL      VABLNP = 089
046700041130     C                   EVAL      VATLNP = 089
046800040706     C                   EVAL      VABCTR = 000
046900040119     C                   EVAL      VABCBO = '1'
047000041130     C                   EVAL      VABTSP = 'C'
047100041130     C                   EVAL      VATTRC = 'E'
047200030822     C*
047300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
047400030715     C* e delimitatore testo.
047500041130     C                   EVAL      CharCSV = ','
047600041130     C                   EVAL      CharTXT = '"'
047700041130     C                   EVAL      CharNUM = '.'
047800041130     C*
047900041130     C* Compongo la sequenza separatore testo + separatore campo
048000041130     C                   EVAL      wOldSep = CharTXT + CharCSV + CharTXT
048100041130     C                   EVAL      wNewSep = CharTXT + ';' + CharTXT
048200030822     C*
048300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
048400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
048500030715     C                   SETON                                        50
048600030715     C                   ELSE
048700030715     C                   SETOFF                                       50
048800030715     C                   ENDIF
048900030822     C*
049000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
049100030822     C                   EVAL      posDaDft = 1
049200030822     C                   EVAL      posADft  = 0
049300030822     C                   EVAL      wGiroDft = 0
049400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
049500030822     C                             posDaDft > 0
049600030822     C*
049700030822     C* Gestisco il 1° giro
049800030822     C                   IF        wGiroDft = 0
049900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
050000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
050100030822     C* Incremento il contatore dei "giri"
050200030822     C                   EVAL      wGiroDft = 1
050300030822     C                   ELSE
050400030822     C                   EVAL      posDaDft = posADft
050500030822     C                   ENDIF
050600030822     C* Eseguo lo scan x trovare la fine del campo corrente
050700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
050800030822     C*
050900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
051000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
051100030822     C                   IF        posDaDft > 0 AND
051200030822     C                             posADft  > 0
051300030822     C* NCL
051400030822     C                   IF        %subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):1:3)
051700030822     C                             = 'NCL'
051800030822     C                   EVAL      PiStr=%trim(%subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052100030822     C                   EXSR      CHKNUM
052200030822     C                   IF        PiInt=*on
052300030822     C                   Z-ADD     PiVal         VABNCL
052400030822     C                   ENDIF
052500030822     C                   ENDIF
052600030822     C* CCM
052700030822     C                   IF        %subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):1:3)
053000030822     C                             = 'CCM'
053100030822     C                   EVAL      PiStr=%trim(%subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053400030822     C                   EXSR      CHKNUM
053500030822     C                   IF        PiInt=*on
053600030822     C                   Z-ADD     PiVal         VABCCM
053700040119     C                   Z-ADD     PiVal         VATCCM
053800030822     C                   ENDIF
053900030822     C                   ENDIF
054000030822     C* LNP
054100030822     C                   IF        %subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):1:3)
054400030822     C                             = 'LNP'
054500030822     C                   EVAL      PiStr=%trim(%subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054800030822     C                   EXSR      CHKNUM
054900030822     C                   IF        PiInt=*on
055000030822     C                   Z-ADD     PiVal         VABLNP
055100040119     C                   Z-ADD     PiVal         VATLNP
055200030822     C                   ENDIF
055300030822     C                   ENDIF
055400030822     C* NRS
055500030822     C                   IF        %subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):1:3)
055800030822     C                             = 'NRS'
055900030822     C                   EVAL      PiStr=%trim(%subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200030822     C                   EXSR      CHKNUM
056300030822     C                   IF        PiInt=*on
056400030822     C                   Z-ADD     PiVal         VABNRS
056500030822     C                   ENDIF
056600030822     C                   ENDIF
056700030822     C* CTR
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'CTR'
057200030822     C                   EVAL      PiStr=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   EXSR      CHKNUM
057600030822     C                   IF        PiInt=*on
057700030822     C                   Z-ADD     PiVal         VABCTR
057800030822     C                   ENDIF
057900030822     C                   ENDIF
058000030822     C* PKB
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'PKB'
058500030822     C                   EVAL      PiStr=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   EXSR      CHKNUM
058900030822     C                   IF        PiNum=*on
059000030822     C                   Z-ADD     PiVal         VABPKB
059100030822     C                   ENDIF
059200030822     C                   ENDIF
059300030822     C* VLB
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'VLB'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiNum=*on
060300030822     C                   Z-ADD     PiVal         VABVLB
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* QFT
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'QFT'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiNum=*on
061600030822     C                   Z-ADD     PiVal         VABQFT
061700030822     C                   ENDIF
061800030822     C                   ENDIF
061900030822     C* CBO
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'CBO'
062400030822     C                   EVAL      VABCBO=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   ENDIF
062800030822     C* TSP
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'TSP'
063300030822     C                   EVAL      VABTSP=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* VAS
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'VAS'
064200030822     C                   EVAL      VABVAS=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* VCA
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'VCA'
065100030822     C                   EVAL      VABVCA=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* GCA
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'GCA'
066000030822     C                   EVAL      VABGCA=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* CTM
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'CTM'
066900030822     C                   EVAL      VABCTM=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* FFD
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'FFD'
067800030822     C                   EVAL      VABFFD=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* VAD
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'VAD'
068700030822     C                   EVAL      VABVAD=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* GMA
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'GMA'
069600030822     C                   EVAL      VABGMA=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* GGA
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'GGA'
070500030822     C                   EVAL      VABGGA=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* GVA
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'GVA'
071400030822     C                   EVAL      VABGVA=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* TC1
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'TC1'
072300030822     C                   EVAL      VABTC1=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* TC2
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'TC2'
073200030822     C                   EVAL      VABTC2=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* ...
073700030822     C                   ENDIF
073800030822     C                   ENDDO
073900020204     C*
074000010330     C                   ENDSR
074100010607     C*----------------------------------------------------*
074200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
074300010607     C*----------------------------------------------------*
074400010607     C     IMPVAB        BEGSR
074500010607     C*
074600010607     C                   SETOFF                                       3132
074700030515     C*
074800010607     C                   MOVEL     datcor        VABAAS
074900040119     C                   MOVEL     datcor        VATAAS
075000010607     C                   MOVE      datcor        VABMGS
075100041130     C*
075200041130     C* Salvo sempre la riga originale x le eventuali successive sovrapposizioni
075300041130     C                   EVAL      depvinDTA = vinDTA
075400041130     C*
075500041130     C* Dato che c'è 1 conflitto d separatori campi/barcodes => sostituisco quello campi
075600041130     C                   EVAL      CharCSV = ';'
075700041130     C*
075800041130     C                   Z-ADD     1             wj                5 0
075900041130     C                   DOW       wj <= %len(vindta) - %len(wOldSep)
076000041130     C                   IF        %subst(vindta:wj:3) = wOldSep
076100041130     C                   EVAL      %subst(vindta:wj:3) = wNewSep
076200041130     C                   ENDIF
076300041130     C                   ADD       1             wj
076400041130     C                   ENDDO
076500010607     C*
076600030715     C**********
076700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076800030715     C**********
076900030715     C                   IF        wGiro = *zeros
077000040113     C*
077100041130     C* Se necessario inserisco qui la forzatura della intesatazione colonne personalizzata
077200041130     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
077300041130     C                             %trim(intcol(3))+%trim(intcol(4)) <> *blanks
077400041130     C*
077500041130     C* Effettuo la sovrapposizione d intestazioni
077600041130     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
077700041130     C                                      %trim(intcol(3))+%trim(intcol(4))
077800041130     C                   ENDIF
077900041130     C*
078000041130     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
078100041130     C                   CALL      'XSPLIT'
078200041130     C                   PARM      vindta        InStringa
078300041130     C                   PARM      CharCSV       InSepara
078400041130     C*                  PARM                    OutArray
078500041130     C                   PARM                    wSkParam
078600041130     C                   PARM                    OutErrore
078700041130     C                   PARM                    OutMsg
078800041130     C                   MOVEA     wSkParam      SkSplitWRK
078900041130     C*
079000041130     C                   EVAL      i = 1
079100041130     C                   DOW       i <= %elem(SkSplitWRK)
079200041130     C                   EVAL      SkSplitFLD(i) = SkSplitWRK(i)
079300041130     C                   ADD       1             i
079400041130     C                   ENDDO
079500030715     C*
079600041130     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079700030715     C* delimitazione testo
079800030715     C                   EVAL      i = 1
079900041130     C                   DOW       i <= %elem(SkSplitFLD)
080000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
080100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
080200030715     C                   EVAL      i = i + 1
080300030715     C                   ENDDO
080400030715     C                   EVAL      wGiro = 1
080500030715     C                   ELSE
080600030715     C                   EVAL      wGiro = 2
080700030715     C**********
080800030715     C* Effettuo lo split del campo dati d input
080900030715     C**********
081000041130     C*
081100041130     C* Chiamo il *pgm d utilità x lo "split" della riga dati corrente
081200041130     C                   CALL(e)   'XSPLIT'
081300041130     C                   PARM      vindta        InStringa
081400041130     C                   PARM      CharCSV       InSepara
081500041130     C*                  PARM                    OutArray
081600041130     C                   PARM                    wSkParam
081700041130     C                   PARM                    OutErrore
081800041130     C                   PARM                    OutMsg
081900041130     C                   MOVEA     wSkParam      SkSplitWRK
082000041130     C                   MOVEL     SkSplitWRK    SkSplitCSV
082100030715     C*
082200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082300030715     C* delimitazione testo
082400030715     C                   EVAL      i = 1
082500041130     C                   DOW       i <= %elem(SkSplitCSV)
082600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
082700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
082800030715     C                   EVAL      i = i + 1
082900030715     C                   ENDDO
083000030715     C*
083100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
083200030715     C                   EVAL      i = 1
083300041130     C                   DOW       i <= %elem(SkSplitFLD)
083400030715     C*
083500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
083600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
083700030715     C                             SkSplitCSV(i) <> *blanks
083800030721     C*** ATB
083900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
084000030721     C                   EVAL      VABATB = SkSplitCSV(i)
084100030721     C                   ENDIF
084200030715     C*** RSD
084300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
084400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
084500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
084600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
084700030715     C* ==
084800030715     C                   ENDIF
084900030721     C*** RD2
085000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
085100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
085200030721     C                   ENDIF
085300030715     C*** IND
085400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
085500030715     C                   EVAL      VABIND = SkSplitCSV(i)
085600030715     C                   ENDIF
085700030715     C*** LOD
085800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
085900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
086000030715     C                   ENDIF
086100030715     C*** PRD
086200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
086300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
086400030715     C                   ENDIF
086500030721     C*** NZD
086600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
086700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
086800030721     C                   ENDIF
086900030715     C*** RMA
087000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
087100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
087200030715     C                   ENDIF
087300030715     C*** NOT
087400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
087500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
087600030715     C                   ENDIF
087700030715     C*** NT2
087800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
087900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
088000030715     C                   ENDIF
088100030715     C*** VCA
088200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
088300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
088400030715     C                   ENDIF
088500030715     C*** VAS
088600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
088700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
088800030715     C                   ENDIF
088900030715     C*** FFD
089000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
089100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
089200030715     C                   ENDIF
089300030721     C*** GC1
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
089500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030721     C*** GC2
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
089900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
090000030721     C                   ENDIF
090100030721     C*** TSP
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
090300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030721     C*** NAS
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
090700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
090800030721     C                   ENDIF
090900030721     C*** TIC
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
091100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030721     C*** GCA
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
091500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030721     C*** XCO
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
091900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030721     C*** CTM
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
092300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
092400030721     C                   ENDIF
092500030721     C*** TCR
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
092700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030721     C*** CTS
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
093100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
093200030721     C                   ENDIF
093300030721     C*** FTM
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
093500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
093600030721     C                   ENDIF
093700030721     C*** VAD
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
093900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030721     C*** GMA
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
094300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030721     C*** GGA
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
094700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030721     C*** GVA
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
095100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
095200030721     C                   ENDIF
095300030721     C*** TC1
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
095500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030721     C*** TC2
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
095900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030721     C*** SCL
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
096300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030721     C*** RMO
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
096700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030721     C*** NMO
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
097100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
097200030721     C                   ENDIF
097300041130     C*
097400040706     C* Reperisco quindi i campi numerici...
097500040706     C*** NSP
097600040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
097700040706     C                   EVAL      PiStr=SkSplitCSV(i)
097800040706     C                   EXSR      CHKNUM
097900041130     C                   IF        PiInt=*on AND PiVal <> *zeros
098000040706     C                   Z-ADD     PiVal         VABNSP
098100040706     C                   ELSE
098200041130     C* NSP => Se valore passato dal clinte nn è valodo => stacco un numeratore da AZNUM
098300041130     C                   clear                   TRUL33DS
098400041130     C                   eval      I33OPE = *zeros
098500041130     C                   eval      I33CNU = 302
098600041130     C                   eval      I33NUM = 1
098700041130     C                   movel     TRUL33DS      KPJBU
098800041130     C                   call      'TRUL33R'
098900041130     C                   parm                    KPJBA
099000041130     C                   movel     KPJBU         TRUL33DS
099100041130     C                   if        O33ERR = *zeros
099200041130     C                   z-add     O33NRF        VABNSP
099300041130     C                   else
099400041130     C                   seton                                        31
099500041130     C                   EVAL      VABNSP = *zeros
099600041130     C                   EVAL      vinmsg = %trimr(vinmsg)
099700041130     C                             + ' ' + 'VABNSP VATNSP'
099800041130     C                   endif
099900041130     C*
100000040706     C                   ENDIF
100100040706     C                   ENDIF
100200040706     C*** RMN
100300040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
100400040706     C                   EVAL      PiStr=SkSplitCSV(i)
100500040706     C                   EXSR      CHKNUM
100600040706     C                   IF        PiInt=*on
100700040706     C                   Z-ADD     PiVal         VABRMN
100800040706     C                   ELSE
100900040706     C                   SETON                                        32
101000040706     C                   EVAL      VABRMN = 1
101100040706     C                   EVAL      vinmsg = %trimr(vinmsg)
101200040706     C                             + ' ' + 'VABRMN'
101300040706     C                   ENDIF
101400040706     C                   ENDIF
101500030721     C*** CAD
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
101700030721     C                   EVAL      PiStr=SkSplitCSV(i)
101800030721     C                   EXSR      CHKNUM
101900030721     C                   IF        PiInt=*on
102000030721     C                   Z-ADD     PiVal         Num5_0
102100030721     C                   MOVEL(p)  Num5_0        VABCAD
102200030721     C                   ELSE
102300030721     C                   SETON                                        32
102400030721     C                   EVAL      VABCAD = *zeros
102500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102600030721     C                             + ' ' + 'VABCAD'
102700030721     C                   ENDIF
102800030721     C                   ENDIF
102900030721     C*** NCL
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
103100030721     C                   EVAL      PiStr=SkSplitCSV(i)
103200030721     C                   EXSR      CHKNUM
103300030721     C                   IF        PiInt=*on
103400030721     C                   Z-ADD     PiVal         VABNCL
103500030721     C                   ELSE
103600030721     C                   SETON                                        32
103700030721     C                   EVAL      VABNCL = *zeros
103800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103900040706     C                             + ' ' + 'VABNCL'
104000030721     C                   ENDIF
104100030721     C                   ENDIF
104200030721     C*** PKB
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
104400030721     C                   EVAL      PiStr=SkSplitCSV(i)
104500030721     C                   EXSR      CHKNUM
104600030721     C                   IF        PiNum=*on
104700030721     C                   Z-ADD     PiVal         VABPKB
104800030721     C                   ELSE
104900030721     C                   SETON                                        32
105000030721     C                   EVAL      VABPKB = *zeros
105100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105200030721     C                             + ' ' + 'VABPKB'
105300030721     C                   ENDIF
105400030721     C                   ENDIF
105500030721     C*** CAS
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
105700030721     C                   EVAL      PiStr=SkSplitCSV(i)
105800030721     C                   EXSR      CHKNUM
105900030721     C                   IF        PiNum=*on
106000030721     C                   Z-ADD     PiVal         VABCAS
106100030721     C                   ELSE
106200030721     C                   SETON                                        32
106300030721     C                   EVAL      VABCAS = *zeros
106400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106500030721     C                             + ' ' + 'VABCAS'
106600030721     C                   ENDIF
106700030721     C                   ENDIF
106800030721     C*** LNP
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
107000030721     C                   EVAL      PiStr=SkSplitCSV(i)
107100030721     C                   EXSR      CHKNUM
107200030721     C                   IF        PiInt=*on    AND
107300030721     C                             PiVal<=999   AND
107400030721     C                             PiVal>*zeros
107500030721     C                   Z-ADD     PiVal         VABLNP
107600040119     C                   Z-ADD     PiVal         VATLNP
107700030721     C                   ELSE
107800030721     C                   SETON                                        32
107900030721     C                   EVAL      VABLNP = *zeros
108000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108100040119     C                             + ' ' + 'VABLNP VATLNP'
108200030721     C                   ENDIF
108300030721     C                   ENDIF
108400030721     C*** NRS
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
108600030721     C                   EVAL      PiStr=SkSplitCSV(i)
108700030721     C                   EXSR      CHKNUM
108800030721     C                   IF        PiInt=*on AND
108900030721     C                             PiVal<=99
109000030721     C                   Z-ADD     PiVal         VABNRS
109100030721     C                   ELSE
109200030721     C                   SETON                                        32
109300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109400030721     C                             + ' ' + 'VABNRS'
109500030721     C                   ENDIF
109600030721     C                   ENDIF
109700030721     C*** LNA
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
109900030721     C                   EVAL      PiStr=SkSplitCSV(i)
110000030721     C                   EXSR      CHKNUM
110100030721     C                   IF        PiInt=*on    AND
110200030721     C                             PiVal<=999   AND
110300030721     C                             PiVal>*zeros
110400030721     C                   Z-ADD     PiVal         VABLNA
110500030721     C                   ELSE
110600030721     C                   SETON                                        32
110700030721     C                   EVAL      VABLNA = *zeros
110800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110900030721     C                             + ' ' + 'VABLNA'
111000030721     C                   ENDIF
111100030721     C                   ENDIF
111200030721     C*** CTR
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiInt=*on  AND
111700030721     C                             PiVal<=999
111800030721     C                   Z-ADD     PiVal         VABCTR
111900030721     C                   ELSE
112000030721     C                   SETON                                        32
112100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112200030721     C                             + ' ' + 'VABCTR'
112300030721     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** IAS
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
112700030721     C                   EVAL      PiStr=SkSplitCSV(i)
112800030721     C                   EXSR      CHKNUM
112900030721     C                   IF        PiNum=*on
113000030721     C                   Z-ADD     PiVal         VABIAS
113100030721     C                   ELSE
113200030721     C                   SETON                                        32
113300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113400030721     C                             + ' ' + 'VABIAS'
113500030721     C                   ENDIF
113600030721     C                   ENDIF
113700030721     C*** VLB
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100030721     C                   IF        PiNum=*on
114200030721     C                   Z-ADD     PiVal         VABVLB
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600030721     C                             + ' ' + 'VABVLB'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900030721     C*** QFT
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
115100030721     C                   EVAL      PiStr=SkSplitCSV(i)
115200030721     C                   EXSR      CHKNUM
115300030721     C                   IF        PiNum=*on
115400030721     C                   Z-ADD     PiVal         VABQFT
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABQFT'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** NCD
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on      AND
116600030721     C                             PiVal<=9999999
116700030721     C                   Z-ADD     PiVal         VABNCD
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100030721     C                             + ' ' + 'VABNCD'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** NCA
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on      AND
117900030721     C                             PiVal<=9999999
118000030721     C                   Z-ADD     PiVal         VABNCA
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABNCA'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** ZNC
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on AND
119200030721     C                             PiVal<=99
119300030721     C                   Z-ADD     PiVal         VABZNC
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700030721     C                             + ' ' + 'VABZNC'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030721     C*** DCR
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiInt=*on       AND
120500030721     C                             PiVal<=99999999
120600030721     C                   Z-ADD     PiVal         VABDCR
120700030721     C                   ELSE
120800030721     C                   SETON                                        32
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABDCR'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** HCR
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on   AND
121800030721     C                             PiVal<=9999
121900030721     C                   Z-ADD     PiVal         VABHCR
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABHCR'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** VMD
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiNum=*on
123100030721     C                   Z-ADD     PiVal         VABVMD
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABVMD'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** ANT
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on        AND
124300030721     C                             PiVal<=999999999
124400030721     C                   Z-ADD     PiVal         VABANT
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABANT'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** CMO
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiInt=*on
125600030721     C                   Z-ADD     PiVal         Num5_0
125700030721     C                   MOVEL(p)  Num5_0        VABCMO
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      VABCMO = *zeros
126100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126200030721     C                             + ' ' + 'VABCMO'
126300030721     C                   ENDIF
126400030721     C                   ENDIF
126500040706     C*
126600041130     C*** VATNOT
126700041130     C                   IF        %trim(SkSplitFLD(i)) = 'BARCODES'
126800041130     C                   EVAL      InStringa=SkSplitCSV(i)
126900041130     C                   CLEAR                   SkSplitNOT
127000041130     C*
127100041130     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
127200041130     C                   CALL(e)   'XSPLIT'
127300041130     C                   PARM                    InStringa
127400041130     C                   PARM      ','           InSepara
127500041130     C*                  PARM                    OutArray
127600041130     C                   PARM                    wSkParam
127700041130     C                   PARM                    OutErrore
127800041130     C                   PARM                    OutMsg
127900041130     C                   MOVEA     wSkParam      SkSplitWRK
128000041130     C*
128100041130     C                   IF        %error
128200041130     C                   SETON                                        32
128300041130     C                   EVAL      vinmsg = %trimr(vinmsg)
128400041130     C                             + ' ' + 'VATNOT'
128500041130     C                   ELSE
128600041130     C* Valorizzo la schiera dei "CHI SONO" x la spedizione corrente
128700041130     C                   EVAL      y = 1
128800041130     C                   DOW       y <= %elem(SkSplitWRK)
128900041130     C                   EVAL      SkSplitNOT(y) = SkSplitWRK(y)
129000041130     C                   ADD       1             y
129100041130     C                   ENDDO
129200041130     C                   ENDIF
129300041130     C*
129400040719     C                   ENDIF
129500030715     C*
129600030715     C                   ENDIF
129700030715     C                   EVAL      i = i + 1
129800030715     C                   ENDDO
129900040706     C*
130000040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
130100040706     C* Reperisco la provincia dal CAP e dalla località
130200040706     C                   IF        VABCAD <> *blanks
130300040706     C                   CLEAR                   TISI95DS
130400041130     C                   EVAL      I95TCN = '4'
130500040706     C                   Z-ADD     datcor        I95DAT
130600040706     C                   EVAL      I95CAP = VABCAD
130700040706     C                   EVAL      I95LOC = VABLOD
130800040706     C                   CALL      'TISI95R'
130900040706     C                   PARM                    TISI95DS
131000040706     C                   EVAL      VABPRD = O95PRV
131100040706     C                   ENDIF
131200040719     C*
131300040719     C* Se NSP nn valorizzato considero errore bloccante in traduzione
131400040719     C                   IF        VABNSP = *zeros
131500040719     C                   SETON                                        31
131600041130     C                   EVAL      vinmsg = %trimr(vinmsg)
131700041130     C                             + ' ' + 'VABNSP VATNSP'
131800040719     C                   ENDIF
131900040719     C*
132000040719     C* Se LNP nn valorizzato considero errore bloccante in traduzione
132100040719     C                   IF        VABLNP = *zeros
132200040719     C                   SETON                                        31
132300041130     C                   EVAL      vinmsg = %trimr(vinmsg)
132400041130     C                             + ' ' + 'VABLNP'
132500040719     C                   ENDIF
132600040706     C*
132700040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
132800040706     C* d valorizzarli sempre entrambi
132900040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
133000040706     C                   Z-ADD     VABNSP        VABRMN
133100040706     C                   ENDIF
133200040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
133300040706     C                   MOVEL     VABRMN        VABRMA
133400040706     C                   ENDIF
133500040113     C*
133600040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
133700040113     C                   IF        VABCAS > *zeros
133800040119     C                   EVAL      VABVCA = 'EUR'
133900040119     C                   IF        VABCBO = '1'
134000040113     C                   EVAL      VABCBO = '4'
134100040119     C                   ENDIF
134200040119     C                   IF        VABCBO = '2'
134300040119     C                   EVAL      VABCBO = '6'
134400040119     C                   ENDIF
134500040113     C                   ENDIF
134600020204     C*
134700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
134800020204     C                   EXSR      CHKIMPDIV
134900030715     C                   ENDIF
135000041130     C*
135100041130     C* Ripristino la riga originale
135200041130     C                   EVAL      vinDTA = depvinDTA
135300020204     C*
135400010607     C                   ENDSR
135500010607     C*----------------------------------------------------*
135600010601
135700020204
135800020204     C*----------------------------------------------------*
135900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
136000020204     C*----------------------------------------------------*
136100020204     C     CHKIMPDIV     BEGSR
136200020204     C*
136300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
136400020204     C                   Z-ADD     *zeros        wrkDec            9 9
136500020204     C*
136600020204     C* Come prima cosa effettuo considerazioni sulla divisa
136700020204     C                   IF        vabIAS > *zeros
136800020204     C                   IF        vabVAS <> 'EUR'
136900020204     C                   EVAL      vabVAS =  'ITL'
137000020204     C                   ENDIF
137100020204     C                   ENDIF
137200020204     C*
137300020204     C                   IF        vabCAS > *zeros
137400020204     C                   IF        vabVCA <> 'EUR'
137500020204     C                   EVAL      vabVCA =  'ITL'
137600020204     C                   ENDIF
137700020204     C                   ENDIF
137800020204     C*
137900020204     C                   IF        vabVMD > *zeros
138000020204     C                   IF        vabVAD <> 'EUR'
138100020204     C                   EVAL      vabVAD =  'ITL'
138200020204     C                   ENDIF
138300020204     C                   ENDIF
138400020204     C*
138500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
138600020204     C                   Z-ADD     vabIAS        wrkDec
138700020204     C                   IF        wrkDec > *zeros
138800020204     C                   IF        vabVAS = 'ITL'
138900020204     C                   EVAL      vabIAS = *zeros
139000020204     C                   ENDIF
139100020204     C                   ENDIF
139200020204     C*
139300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
139400020204     C                   Z-ADD     vabCAS        wrkDec
139500020204     C                   IF        wrkDec > *zeros
139600020204     C                   IF        vabVCA = 'ITL'
139700020204     C                   EVAL      vabCAS = *zeros
139800020204     C                   ENDIF
139900020204     C                   ENDIF
140000020204     C*
140100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
140200020204     C                   Z-ADD     vabVMD        wrkDec
140300020204     C                   IF        wrkDec > *zeros
140400020204     C                   IF        vabVAD = 'ITL'
140500020204     C                   EVAL      vabVMD = *zeros
140600020204     C                   ENDIF
140700020204     C                   ENDIF
140800020204     C*
140900020204     C                   ENDSR
141000020204     C***
141100020204
141200010330
141300010330
141400010330     C*----------------------------------------------------*
141500010330     C*  CONTROLLO NUMERICITA' CAMPI
141600010330     C*----------------------------------------------------*
141700010330     C     CHKNUM        BEGSR
141800010330     C*
141900010606     C                   IF        PiDecChr = *blanks
142000030715     C                   EVAL      PiDecChr = CharNUM
142100010606     C                   ENDIF
142200010606     C*
142300010606     C                   CALL(e)   'ISNUMERIC'
142400010330     C                   PARM                    PiStr            30
142500010606     C                   PARM                    PiDecChr          1
142600010330     C                   PARM      *ZEROS        PiVal            30 9
142700010330     C                   PARM      '0'           PiInt             1
142800010330     C                   PARM      '0'           PiNum             1
142900010330     C                   IF        %error
143000010606     C                   EVAL      PiNum=*off
143100010330     C                   ENDIF
143200010330     C*
143300010330     C                   ENDSR
143400010330     C***
143500010330
143600010601
143700010601
143800010601
143900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
144000010601     C     repfil        BEGSR
144100010601     C*
144200010601     C                   if        invfil = *zeros and
144300010601     C                             depfil > *zeros and
144400010629     C                             (vinflg = *blanks or
144500010629     C                              vinflg = *zeros)
144600010601     C
144700010601     C                   eval      invfil = depfil
144800010601     C                   endif
144900010601     C*
145000010601     C                   if        depfil <> invfil and
145100010601     C                             invfil > *zeros
145200010601     C                   eval      flgMulti = '1'
145300010601     C                   if        vinflg = *blanks
145400010601     C                   add       1             cntNonEl
145500010601     C                   endif
145600010601     C                   endif
145700010601     C*
145800010601     C                   if        vinflg = '2'
145900010601     C                   eval      flgStato = '2'
146000010601     C                   endif
146100010601     C*
146200010601     C                   ENDSR
146300010601     C***
146400010601
146500010601
146600010601
146700010330
146800040119      /TITLE Invio dei dati al punto operativo.
146900040119     C     invio         BEGSR
147000040119     C*
147100040119     C* 1° invio FIVAT
147200040119     C                   reset                   dscmz
147300040119     C                   move      vlrpoi        cmzdst
147400040119     C                   eval      cmzfld = 'FIVATWWR'
147500040119     C                   eval      cmzmbd = vlrhdl
147600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
147700040119     C***                if        prmfir = *blanks
147800040119     C                   eval      cmzfla = 'FIVAT00F'
147900040119     C                   eval      cmzmba = 'FIVAT00F'
148000040119     C***                else
148100040119     C***                eval      cmzfla = prmfir
148200040119     C***                eval      cmzmba = prmfir
148300040119     C***                endif
148400040119     C                   eval      cmznrr = *zeros
148500040119     C                   move      §ctrokvt      cmznrr
148600040119     C                   eval      cmzlba = vlrfl1
148700040119     C                   call(e)   'TIS711C'
148800040119     C                   parm                    dscmz
148900040119     C                   parm      *blanks       esito
149000040119     C                   if        %error
149100040119     C                             or cmzerr = '1'
149200040119     C                             or esito  = '1'
149300040119     C                   eval      wrkesito = '3'
149400040119     C                   else
149500040119     C*
149600040119     C* 2° invio FIVAB
149700040119     C                   reset                   dscmz
149800040119     C                   move      vlrpoi        cmzdst
149900040119     C                   eval      cmzfld = vlrfou
150000040119     C                   eval      cmzmbd = vlrhdl
150100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
150200040119     C***                if        prmfir = *blanks
150300040119     C                   eval      cmzfla = 'FIVAB00F'
150400040119     C                   eval      cmzmba = 'FIVAB00F'
150500040119     C***                else
150600040119     C***                eval      cmzfla = prmfir
150700040119     C***                eval      cmzmba = prmfir
150800040119     C***                endif
150900040119     C                   eval      cmznrr = *zeros
151000040119     C                   move      §ctrokvb      cmznrr
151100040119     C                   eval      cmzlba = vlrfl1
151200040119     C                   call(e)   'TIS711C'
151300040119     C                   parm                    dscmz
151400040119     C                   parm      *blanks       esito
151500040119     C                   if        %error
151600040119     C                             or cmzerr = '1'
151700040119     C                             or esito  = '1'
151800040119     C                   eval      wrkesito = '3'
151900040119     C                   endif
152000040119     C                   endif
152100040119     C*
152200040119     C                   ENDSR
152300040119     C***
152400010601
152500010601
152600010601
152700010601
152800010601      /TITLE Invio dei dati al punto operativo.
152900010601     C     opeini        BEGSR
153000010601     C*
153100010601     C* Inizializzo flag e contatori operativi
153200010601     C                   movel     '0'           flgGiro           1
153300010601     C                   movel     '0'           flgMulti          1
153400010601     C                   movel     '1'           flgStato          1
153500010615     C                   movel     '0'           flgOk             1
153600010601     C                   z-add     *zeros        cntNonEl         10 0
153700010601     C                   z-add     *zeros        depfil            3 0
153800010601     C                   z-add     *zeros        invfil            3 0
153900010601     C*
154000010601     C                   ENDSR
154100010601     C***
154200010601
154300010601
154400010601
154500070411
154600070411     C     *pssr         BEGSR
154700070411     C*
154800070411     C                   if        %open(tivin00r)
154900070411     C                   close     tivin00r
155000070411     C                   endif
155100070411     C                   if        %open(fivabwwr)
155200070411     C                   close     fivabwwr
155300070411     C                   endif
155400070411     C                   if        %open(fivatwwr)
155500070411     C                   close     fivatwwr
155600070411     C                   endif
155700070411     C*
155800070411     C* Effettuo la chiamata al CLLE preposto
155900070411     C                   call(e)   'TITVVTC'
156000070411     C                   parm                    parccm
156100070411     C                   parm                    parmbr
156200070411     C                   parm      '2'           paropz
156300070411     C*
156400070411     C                   eval      wrkesito = '2'
156500070411     C*
156600070411     C                   seton                                        LR
156700070411     C*
156800070411     C                   ENDSR     '*CANCL'
156900070411     C***
157000010330
157100010330
157200000613     C     *inzsr        BEGSR
157300990910     C*
157400990910     C     *entry        plist
157500990920     C                   parm                    tivlrds
157600990921     C                   parm      wrkesito      esito
157700000724     C                   parm                    prmlit
157800000710     C                   parm                    prmfir
157900010330     C*
158000010330     C* CALCOLA LA DATA CORRENTE
158100010330     C                   time                    wn14             14 0
158200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
158300010330     C                   z-add     wn8           g08dat
158400010330     C                   z-add     *zeros        g08inv
158500010330     C                   movel     '0'           g08err
158600010330     C                   call      'XSRDA8'
158700010330     C                   parm                    wlbda8
158800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
158900000613     C*
159000000613     C                   ENDSR
159100000613     C***
159200990908
159300041130     Otitvg1p   E            riepilogo         2
159400990915     O                                              'Upload via Internet'
159500990915     O                                           +1 'Traduzione TIVIN00R -'
159600021025     O                                           +1 'FIVABWWR'
159700990915     O                                           +1 'Testate bolle'
159800990915     O          E            riepilogo   2
159900990915     O                       wrkdata
160000990915     O                       wrkora              +1
160100990915     O                       procname            +1
160200990915     O          E            riepilogo   2
160300990915     O                                              'Cliente..................:'
160400010605     O                       VABCCM        z     +1
160500990915     O          E            riepilogo   2
160600990920     O                                              'Riferimento Strategi.....:'
160700990920     O                       vlrhdl              +1
160800990915     O          E            riepilogo   2
160900990915     O                                              'Giusti...................:'
161000040119     O                       §CTROKVB      2   +  1
161100990915     O          E            riepilogo   2
161200990915     O                                              'Sbagliati e corretti.....:'
161300971022     O                       §CTRMO        2   +  1
161400990915     O          E            riepilogo   2
161500990915     O                                              'Sbagliati e scartati.....:'
161600971022     O                       §CTRNO        2   +  1
161700000613
161800041130     Otitvg1ps  E            testdett          2
161900000613     O                                              'Upload via Internet'
162000000613     O                                           +1 'Traduzione TIVIN00R -'
162100021025     O                                           +1 'FIVABWWR'
162200000613     O                                           +1 'Testate bolle'
162300000616     O          E            testdett    3
162400000613     O                                           +2 'N° rec'
162500000613     O                                           +3 'Anteprima contenuto'
162600000616     O          E            rigadett    2
162700000613     O                       rrnum               +2
162800000621     O                       recko               +3
162900000616     O          E            findett     2
163000000613     O                       wrkdata
163100000613     O                       wrkora              +1
163200000613     O                       procname            +1
163300000616     O          E            findett     2
163400000613     O                                              'Cliente..................:'
163500010605     O                       VABCCM        z     +1
163600000616     O          E            findett     2
163700000613     O                                              'Riferimento Strategi.....:'
163800000613     O                       vlrhdl              +1
163900000616     O          E            findett     2
164000000613     O                                              'Giusti...................:'
164100040119     O                       §CTROKVB      2   +  1
164200000616     O          E            findett     2
164300000613     O                                              'Sbagliati e corretti.....:'
164400000613     O                       §CTRMO        2   +  1
164500000616     O          E            findett     2
164600000613     O                                              'Sbagliati e scartati.....:'
164700000613     O                       §CTRNO        2   +  1
164800000613** CMD - COMANDI CL
164900041130OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVG1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
165000041130OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVG1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
165100041130DLTOVR FILE(TITVG1P TITVG1PS) LVL(*)
165200010606
165300000613
165400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
165500041130"FILLER1";"FILLER2";"FILLER3";"FILLER4";"VABRSD";"VABIND";"FILLER7";"VABCAD";"VABLOD";
165600041130"FILLER10";"FILLER11";"FILLER12";"VABRMA";"BARCODES";"VABNSP";"FILLER16";"FILLER17";
165700041130"VABNCL";"VABPKB";"FILLER20";"FILLER21";"FILLER22";"FILLER23";"FILLER24";"FILLER25";
165800041130"FILLER26";"FILLER27";"FILLER28";"FILLER29";"FILLER30";"FILLER31"
