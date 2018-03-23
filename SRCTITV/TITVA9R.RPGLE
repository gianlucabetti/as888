000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800040405     Ftitva9p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100040405     Ftitva9ps  O    f  198        PRINTER usropn
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
003900040405     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004000040119     D parccm          s              8    INZ(*blanks)
004100040119     D parmbr          s             10    INZ(*blanks)
004200040119     D paropz          s              1    INZ(*blanks)
004300040119     D chkcall         s              1    INZ(*blanks)
004400040405     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004500040405     D wRMANOT         s             30  0 INZ(*zeros)
004600030715     D*------------------
004700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004800030715     D*------------------
004900030715     D SkSplitFLD      S             10    DIM(100)
005000030715     D SkSplitCSV      S            256    DIM(100)
005100030715     D CharCSV         S              1
005200030715     D CharTXT         S              1
005300030715     D CharNUM         S              1
005400030715     D posDa           S              3  0 INZ(*zeros)
005500030715     D posA            S              3  0 INZ(*zeros)
005600030715     D i               s              3  0 INZ(1)
005700030715     D wGiro           s              1  0 INZ(*zeros)
005800030822     D*------------------
005900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006000030822     D*------------------
006100030822     D posDaDft        S              3  0 INZ(*zeros)
006200030822     D posADft         S              3  0 INZ(*zeros)
006300030822     D j               s              3  0 INZ(1)
006400030822     D wGiroDft        s              1  0 INZ(*zeros)
006500010330     D*------------------
006600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006700010330     D*------------------
006800010330     D WLBDA8          DS                  INZ
006900010330     D  G08DAT                 1      8  0
007000010330     D  G08INV                 9     16  0
007100010330     D  G08ERR                17     17
007200010330     D  G08TGI                18     22  0
007300031201     D*------------------
007400031201     D* Costanti
007500031201     D*------------------
007600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007800010330
007900010330
008000010330
008100990915     C                   time                    wrkdata
008200990915     C                   time                    wrkora
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600010601     C*
008700010601     C                   exsr      opeini
008800010605     C                   exsr      rwvab
008900010601     C*
009000010601     C                   seton                                        lr
009100010601
009200010601
009300010601
009400010601     C*--------------------------------------------------------
009500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009600010601     C*--------------------------------------------------------
009700010601     C     PREELA        BEGSR
009800010601     C*
009900010601     C* SE OCCORRE SPEDIRE IN FILIALE
010000010601     C                   if        invfil <> *zeros and
010100010601     C                             flgGiro = '0'
010200010601     C*
010300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010400010601     C                   eval      flgGiro = '1'
010500000724     C*
010600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010800010601     C     invfil        chain     azorg01l
010900000724     C                   if        %found
011000000724     C                   movel(p)  CMD(1)        depcmd
011100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011200000724     C*
011300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000724     C                   PARM                    depcmd
011700000724     C                   PARM                    LENGH
011800000724     C*
011900000724     C                   endif
012000000616     C*
012100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012200000616     C                   movel(p)  CMD(2)        depcmd
012300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012400000616     C*
012500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012600011113     C                   Z-ADD     150           LENGH            15 5
012700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012800000616     C                   PARM                    depcmd
012900000616     C                   PARM                    LENGH
013000000613     C*
013100040405     C                   if        not %open(titva9ps)
013200040405     C                   open      titva9ps
013300030715     C                   except    testdett
013400000613     C                   endif
013500010601     C*
013600010601     C                   endif
013700010601     C*
013800010601     C                   ENDSR
013900010601     C***
014000010601
014100010601
014200010601
014300010601
014400010601     C*--------------------------------------------------------
014500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014600010601     C*--------------------------------------------------------
014700010601     C     ENDELA        BEGSR
014800990915     C*
014900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015000040119     C*
015100040119     C* Effettuo la chiamata al CLLE preposto
015200040119     C                   call(e)   'TITVVTC'
015300040119     C                   parm                    parccm
015400040119     C                   parm                    parmbr
015500040119     C                   parm      '2'           paropz
015600000613     C*
015700040405     C                   if        %open(titva9ps)
015800000616     C                   except    findett
015900040405     C                   close     titva9ps
016000000613     C                   endif
016100000616     C*
016200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016300011113     C                   Z-ADD     150           LENGH            15 5
016400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
016500000616     C                   PARM                    CMD(3)
016600000616     C                   PARM                    LENGH
016700000616     C*
016800010601     C                   ENDSR
016900010601     C***
017000010601
017100010601
017200010601
017300000613
017400000613     C*--------------------------------------------------------
017500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017600000613     C*--------------------------------------------------------
017700000621     C     STPR          BEGSR
017800000613     C*
017900040405     C                   if        not %open(titva9p)
018000040405     C   50              open      titva9p
018100990915     C                   endif
018200990915     C*
018300030715     C   50              except    riepilogo
018400990915     C*
018500040405     C                   if        %open(titva9p)
018600040405     C   50              close     titva9p
018700990914     C                   endif
018800000613     C*
018900000613     C                   ENDSR
019000000613     C***
019100000613
019200010601
019300010601
019400010330     C*--------------------------------------------------------
019500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
019600010330     C*--------------------------------------------------------
019700010605     C     RWVAB         BEGSR
019800010330     C*
019900010330     C                   if        not %open(tivin00r)
020000010330     C                   open      tivin00r
020100010330     C                   endif
020200021025     C                   if        not %open(fivabwwr)
020300021025     C                   open      fivabwwr
020400010330     C                   endif
020500040119     C*
020600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
020700040119     C                   exsr      prevat
020800040119     C*
020900040119     C                   if        chkcall = '0'
021000040119     C*
021100040119     C                   if        not %open(fivatwwr)
021200040119     C                   open      fivatwwr
021300040119     C                   endif
021400010330     C*
021500040119     C                   clear                   §CTROKVB          7 0
021600040119     C                   clear                   §CTROKVT          7 0
021700040119     C                   clear                   §CTRMO            7 0
021800040119     C                   clear                   §CTRNO            7 0
021900010330     C*
022000030822     C                   do        *HIVAL
022100010330     C*
022200030822     C                   read      tivin00r                               70
022300010618     C*
022400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
022500010618     C                   if        vinflg = '1'
022600010618     C                   eval      flgOk = '1'
022700010618     C                   endif
022800010618     C*
022900040316     C                   if        (vindta > *blanks AND
023000040316     C                              %trim(vindta) <> *all';') OR
023100040316     C                             wGiro = *zeros
023200010330     C                   add       1             rrnum
023300010330     C*
023400010601     C                   if        *in70 = *off and
023500010330     C                             (vinflg = *blanks
023600010330     C                              or vinflg = '0'
023700010330     C                              or vinflg = '2')
023800010330     C*
023900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
024000010711     C                   if        vinflg = *blanks or vinflg = '0'
024100010711     C                   clear                   vinmsg
024200010711     C                   endif
024300010601     C*
024400010330     C                   exsr      inzvar
024500010605     C                   exsr      impvab
024600010601     C*
024700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024800010605     C                   eval      depfil = VABLNP
024900010601     C                   exsr      repfil
025000010601     C                   if        depfil = invfil
025100021025     C                   if        vlrpoi = 999
025200030822     C                   move(P)   invfil        VABFGS
025300040119     C                   move(P)   invfil        VATFGS
025400021025     C                   else
025500030822     C                   move(P)   vlrpoi        VABFGS
025600040119     C                   move(P)   vlrpoi        VATFGS
025700021025     C                   endif
025800010601     C*
025900010601     C                   exsr      PREELA
026000010601     C*
026100010604     C* Ebbene...
026200010604     C*
026300040119     C  N31              add       1             §CTROKVB
026400040119     C   32              add       1             §CTRMO
026500040119     C   31              add       1             §CTRNO
026600030822     C                   if        wGiro = 2
026700040119     C                   if        depspe =  *zeros OR
026800040119     C                             depspe <> vabNSP
026900040119     C  N31              exsr      wrivab
027000040119     C  N31              eval      depspe = vabNSP
027100040119     C                   endif
027200040119     C                   endif
027300010604     C*
027400010604     C                   if        *in31 = *off and
027500010604     C                             *in32 = *off
027600010604     C                   eval      vinflg = '1'
027700010604     C                   else
027800010604     C                   eval      recko = vindta
027900010604     C                   except    rigadett
028000010604     C                   eval      vinflg = '2'
028100010604     C                   endif
028200010604     C                   endif
028300010601     C*
028400010604     C                   endif
028500010604     C*
028600010330     C                   else
028700010330     C                   eval      vinflg = '1'
028800010330     C                   endif
028900010601     C*
029000010601     C  N70              update    tivin000
029100010330     C*
029200030822     C  N70              enddo
029300040119     C*
029400040119     C                   endif                                                  (endif - chkcall)
029500010601     C*
029600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029700010601     C                   if        cntNonEl = *zeros or
029800010601     C                             flgMulti = '0'
029900010330     C* Se non ci sono record con errori ...
030000010601     C                   if        §ctrno = 0 and
030100010604     C                             §ctrmo = 0 and
030200010601     C                             flgStato <> '2'
030300010330     C* ... restituisco esito OK.
030400010330     C                   eval      wrkesito = '0'
030500010330     C                   else
030600040119     C                   if        §ctrokvb > 0
030700010330     C                   eval      wrkesito = '1'
030800010330     C                   else
030900010615     C                   if        flgOk = '0'
031000010615     C                   eval      wrkesito = '2'
031100010615     C                   else
031200010615     C                   eval      wrkesito = '6'
031300010615     C                   endif
031400010330     C                   endif
031500010330     C                   endif
031600010601     C                   else
031700010601     C                   eval      wrkesito = '9'
031800010601     C                   endif
031900010330     C*
032000010330     C                   if        %open(tivin00r)
032100010330     C                   close     tivin00r
032200010330     C                   endif
032300021025     C                   if        %open(fivabwwr)
032400021025     C                   close     fivabwwr
032500010330     C                   endif
032600040512     C                   if        %open(fivatwwr)
032700040512     C                   close     fivatwwr
032800040512     C                   endif
032900010601     C*
033000010601     C                   if        vlrpoi <> 999
033100010601     C                   eval      invfil = vlrpoi
033200010601     C                   endif
033300010330     C*
033400040119     C                   if        §ctrokvb > 0
033500010601     C                             and invfil > *zeros
033600010330     C                   exsr      invio
033700010330     C                   endif
033800010601     C*
033900010618     C                   if        flgGiro = '1'
034000010601     C                   exsr      endela
034100010618     C                   endif
034200010330     C*
034300010330     C                   ENDSR
034400010330     C***
034500040119
034600040119
034700040119
034800040119     C*----------------------------------------------------*
034900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035000040119     C*----------------------------------------------------*
035100040119     C     WRIVAB        BEGSR
035200040119     C*
035300040119     C                   write     fivab000                                     => scarico il VAB
035400040119     C*
035500040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
035600040119     C                   exsr      wrivat
035700040119     C*
035800040119     C                   ENDSR
035900040119
036000040119
036100040119
036200040119     C*----------------------------------------------------*
036300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
036400040119     C*----------------------------------------------------*
036500040119     C     WRIVAT        BEGSR
036600040119     C*
036700040119     C                   write     FIVAT000
036800040119     C                   add       1             §CTROKVT
036900040119     C*
037000040119     C                   ENDSR
037100040119
037200040119
037300040119
037400040119     C*----------------------------------------------------*
037500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
037600040119     C*----------------------------------------------------*
037700040119     C     PREVAT        BEGSR
037800040119     C*
037900040119     C* Compongo il nome del membro da dare al FIVATWWR
038000040119     C                   eval      parmbr = vlrhdl
038100040119     C                   movel     'M'           parmbr
038200040405     C                   eval      parccm = '00892759'
038300040119     C                   eval      paropz = '1'
038400040119     C* Effettuo la chiamata al CLLE preposto
038500040119     C                   call(e)   'TITVVTC'
038600040119     C                   parm                    parccm
038700040119     C                   parm                    parmbr
038800040119     C                   parm                    paropz
038900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
039000040119     C                   if        %error
039100040119     C                   movel     '1'           chkcall
039200040119     C                   else
039300040119     C                   movel     '0'           chkcall
039400040119     C                   endif
039500040119     C*
039600040119     C                   ENDSR
039700010601
039800010601
039900010601
040000010330     C*----------------------------------------------------*
040100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
040200010330     C*----------------------------------------------------*
040300040119     C     INZVAR        BEGSR
040400010330     C*
040500030822     C* Inizializzo variabili di wrk
040600020204     C                   Z-ADD     *zeros        Num5_0            5 0
040700030822     C*
040800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
040900030822     C                   CLEAR                   FIVAB000
041000040119     C                   CLEAR                   FIVAT000
041100030822     C                   CLEAR                   SkSplitCSV
041200030822     C                   EVAL      i = 1
041300030822     C                   EVAL      posDa = *zeros
041400030822     C                   EVAL      posA  = *zeros
041500030822     C*
041600030822     C* Reimposto i valori di default
041700030822     C                   EXSR      DEFCAM
041800030822     C*
041900010330     C                   ENDSR
042000010330     C*----------------------------------------------------*
042100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
042200010330     C*----------------------------------------------------*
042300010330     C     DEFCAM        BEGSR
042400040119     C*
042500040119     C* Imposto i valori d default a programma dei campi fondamentali
042600040405     C                   EVAL      VABCCM = 0892759
042700040405     C                   EVAL      VATCCM = 0892759
042800040405     C                   EVAL      VABLNP = 089
042900040405     C                   EVAL      VATLNP = 089
043000040405     C                   EVAL      VABCTR = 300
043100040119     C                   EVAL      VABCTM = '7Q'
043200040119     C                   EVAL      VABCBO = '1'
043300040405     C                   EVAL      VABTSP = 'C'
043400040119     C                   EVAL      VATTRC = 'E'
043500030822     C*
043600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
043700030715     C* e delimitatore testo.
043800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
043900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
044000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
044100030822     C*
044200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
044300030715     C                   IF        %subst(vlrppt:1:1) = 'S'
044400030715     C                   SETON                                        50
044500030715     C                   ELSE
044600030715     C                   SETOFF                                       50
044700030715     C                   ENDIF
044800030822     C*
044900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
045000030822     C                   EVAL      posDaDft = 1
045100030822     C                   EVAL      posADft  = 0
045200030822     C                   EVAL      wGiroDft = 0
045300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
045400030822     C                             posDaDft > 0
045500030822     C*
045600030822     C* Gestisco il 1° giro
045700030822     C                   IF        wGiroDft = 0
045800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
045900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
046000030822     C* Incremento il contatore dei "giri"
046100030822     C                   EVAL      wGiroDft = 1
046200030822     C                   ELSE
046300030822     C                   EVAL      posDaDft = posADft
046400030822     C                   ENDIF
046500030822     C* Eseguo lo scan x trovare la fine del campo corrente
046600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
046700030822     C*
046800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
046900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
047000030822     C                   IF        posDaDft > 0 AND
047100030822     C                             posADft  > 0
047200030822     C* NCL
047300030822     C                   IF        %subst(
047400030822     C                             %subst(vlrppt:posDaDft+1:
047500030822     C                             posADft-posDaDft-1):1:3)
047600030822     C                             = 'NCL'
047700030822     C                   EVAL      PiStr=%trim(%subst(
047800030822     C                             %subst(vlrppt:posDaDft+1:
047900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048000030822     C                   EXSR      CHKNUM
048100030822     C                   IF        PiInt=*on
048200030822     C                   Z-ADD     PiVal         VABNCL
048300030822     C                   ENDIF
048400030822     C                   ENDIF
048500030822     C* CCM
048600030822     C                   IF        %subst(
048700030822     C                             %subst(vlrppt:posDaDft+1:
048800030822     C                             posADft-posDaDft-1):1:3)
048900030822     C                             = 'CCM'
049000030822     C                   EVAL      PiStr=%trim(%subst(
049100030822     C                             %subst(vlrppt:posDaDft+1:
049200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049300030822     C                   EXSR      CHKNUM
049400030822     C                   IF        PiInt=*on
049500030822     C                   Z-ADD     PiVal         VABCCM
049600040119     C                   Z-ADD     PiVal         VATCCM
049700030822     C                   ENDIF
049800030822     C                   ENDIF
049900030822     C* LNP
050000030822     C                   IF        %subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):1:3)
050300030822     C                             = 'LNP'
050400030822     C                   EVAL      PiStr=%trim(%subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050700030822     C                   EXSR      CHKNUM
050800030822     C                   IF        PiInt=*on
050900030822     C                   Z-ADD     PiVal         VABLNP
051000040119     C                   Z-ADD     PiVal         VATLNP
051100030822     C                   ENDIF
051200030822     C                   ENDIF
051300030822     C* NRS
051400030822     C                   IF        %subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):1:3)
051700030822     C                             = 'NRS'
051800030822     C                   EVAL      PiStr=%trim(%subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052100030822     C                   EXSR      CHKNUM
052200030822     C                   IF        PiInt=*on
052300030822     C                   Z-ADD     PiVal         VABNRS
052400030822     C                   ENDIF
052500030822     C                   ENDIF
052600030822     C* CTR
052700030822     C                   IF        %subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):1:3)
053000030822     C                             = 'CTR'
053100030822     C                   EVAL      PiStr=%trim(%subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053400030822     C                   EXSR      CHKNUM
053500030822     C                   IF        PiInt=*on
053600030822     C                   Z-ADD     PiVal         VABCTR
053700030822     C                   ENDIF
053800030822     C                   ENDIF
053900030822     C* PKB
054000030822     C                   IF        %subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):1:3)
054300030822     C                             = 'PKB'
054400030822     C                   EVAL      PiStr=%trim(%subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054700030822     C                   EXSR      CHKNUM
054800030822     C                   IF        PiNum=*on
054900030822     C                   Z-ADD     PiVal         VABPKB
055000030822     C                   ENDIF
055100030822     C                   ENDIF
055200030822     C* VLB
055300030822     C                   IF        %subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):1:3)
055600030822     C                             = 'VLB'
055700030822     C                   EVAL      PiStr=%trim(%subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000030822     C                   EXSR      CHKNUM
056100030822     C                   IF        PiNum=*on
056200030822     C                   Z-ADD     PiVal         VABVLB
056300030822     C                   ENDIF
056400030822     C                   ENDIF
056500030822     C* QFT
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'QFT'
057000030822     C                   EVAL      PiStr=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   EXSR      CHKNUM
057400030822     C                   IF        PiNum=*on
057500030822     C                   Z-ADD     PiVal         VABQFT
057600030822     C                   ENDIF
057700030822     C                   ENDIF
057800030822     C* CBO
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'CBO'
058300030822     C                   EVAL      VABCBO=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   ENDIF
058700030822     C* TSP
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'TSP'
059200030822     C                   EVAL      VABTSP=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   ENDIF
059600030822     C* VAS
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'VAS'
060100030822     C                   EVAL      VABVAS=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   ENDIF
060500030822     C* VCA
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900030822     C                             = 'VCA'
061000030822     C                   EVAL      VABVCA=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   ENDIF
061400030822     C* GCA
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'GCA'
061900030822     C                   EVAL      VABGCA=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   ENDIF
062300030822     C* CTM
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'CTM'
062800030822     C                   EVAL      VABCTM=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   ENDIF
063200030822     C* FFD
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'FFD'
063700030822     C                   EVAL      VABFFD=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   ENDIF
064100030822     C* VAD
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'VAD'
064600030822     C                   EVAL      VABVAD=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   ENDIF
065000030822     C* GMA
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'GMA'
065500030822     C                   EVAL      VABGMA=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* GGA
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'GGA'
066400030822     C                   EVAL      VABGGA=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800030822     C* GVA
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'GVA'
067300030822     C                   EVAL      VABGVA=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   ENDIF
067700030822     C* TC1
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'TC1'
068200030822     C                   EVAL      VABTC1=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   ENDIF
068600030822     C* TC2
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'TC2'
069100030822     C                   EVAL      VABTC2=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   ENDIF
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
070800040119     C                   MOVEL     datcor        VATAAS
070900010607     C                   MOVE      datcor        VABMGS
071000010607     C*
071100030715     C**********
071200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
071300030715     C**********
071400030715     C                   IF        wGiro = *zeros
071500040405     C*
071600040405     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
071700040405     C                   EVAL      depvinDTA = vinDTA
071800040113     C*
071900040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
072000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
072100040113     C                                      %trim(intcol(3))
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
076800040405     C* Ripristino la 1° riga originale
076900040405     C                   EVAL      vinDTA = depvinDTA
077000040405     C                   ELSE
077100030715     C                   EVAL      wGiro = 2
077200031204     C**********
077300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
077400031204     C* d separatore campo
077500031204     C**********
077600031204     C                   EVAL      vindta = %trim(vindta)
077700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
077800031204     C                   EVAL      vindta = %subst(vindta:2)
077900031204     C                   ENDDO
078000031204     C*
078100031204     C                   Z-ADD     *zeros        lunghInput        4 0
078200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
078300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
078400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
078500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
078600031204     C                   ENDDO
078700030715     C**********
078800030715     C* Effettuo lo split del campo dati d input
078900030715     C**********
079000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
079100030715     C                   DOW       posDa <= %len(%trim(vindta))
079200030715     C*
079300030715     C* Gestisco il 1° campo
079400030715     C                   IF        i = 1
079500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079600030715     C                   EVAL      posDa = 1
079700030715     C* Eseguo lo scan x trovare la fine del primo campo
079800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079900030715     C* A questo punto "estraggo" il campo corrente
080000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080100030715     C                                                    (posA-posDa))
080200030715     C* X i campi successivi al 1°
080300030715     C                   ELSE
080400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080500030715     C                   EVAL      posDa = posA + 1
080600031204     C* Verifico che nn vi sia il campo nn valorizzato
080700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
080800031204     C* Se campo nn valorizzato skippo al prossimo
080900031204     C                   EVAL      SkSplitCSV(i) = *blanks
081000031204     C                   EVAL      posA  = posA + 1
081100031204     C                   ELSE
081200030715     C* Eseguo lo scan x trovare la fine del campo corrente
081300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081400030715     C* Gestisco l'ultimo campo
081500030715     C                   IF        posA = *zeros
081600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081700030715     C* A questo punto "estraggo" il campo corrente
081800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081900030715     C                                                    (posA-posDa))
082000030715     C                   LEAVE
082100030715     C                   ELSE
082200030715     C* A questo punto "estraggo" il campo corrente
082300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082400030715     C                                                    (posA-posDa))
082500030715     C                   ENDIF
082600031204     C                   ENDIF
082700030715     C                   ENDIF
082800030715     C* Incremento il contatore d campo
082900030715     C                   EVAL      i = i +1
083000030715     C                   ENDDO
083100030715     C*
083200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083300030715     C* delimitazione testo
083400030715     C                   EVAL      i = 1
083500030715     C                   DOW       i < %elem(SkSplitCSV)
083600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
083700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
083800030715     C                   EVAL      i = i + 1
083900030715     C                   ENDDO
084000030715     C*
084100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
084200030715     C                   EVAL      i = 1
084300030715     C                   DOW       i < %elem(SkSplitFLD)
084400030715     C*
084500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
084600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
084700030715     C                             SkSplitCSV(i) <> *blanks
084800030721     C*** ATB
084900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
085000030721     C                   EVAL      VABATB = SkSplitCSV(i)
085100030721     C                   ENDIF
085200030715     C*** RSD
085300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
085400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
085500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
085600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
085700030715     C* ==
085800030715     C                   ENDIF
085900030721     C*** RD2
086000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
086100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
086200030721     C                   ENDIF
086300030715     C*** IND
086400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
086500030715     C                   EVAL      VABIND = SkSplitCSV(i)
086600030715     C                   ENDIF
086700030715     C*** LOD
086800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
086900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
087000030715     C                   ENDIF
087100030715     C*** PRD
087200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
087300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
087400030715     C                   ENDIF
087500030721     C*** NZD
087600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
087700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
087800030721     C                   ENDIF
087900030715     C*** RMA
088000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
088100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
088200030715     C                   ENDIF
088300030715     C*** NOT
088400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
088500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
088600030715     C                   ENDIF
088700030715     C*** NT2
088800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
088900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
089000030715     C                   ENDIF
089100030715     C*** VCA
089200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
089300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
089400030715     C                   ENDIF
089500030715     C*** VAS
089600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
089700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
089800030715     C                   ENDIF
089900030715     C*** FFD
090000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
090100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
090200030715     C                   ENDIF
090300030721     C*** GC1
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
090500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** GC2
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
090900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** TSP
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
091300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
091400030721     C                   ENDIF
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
098300040119     C*
098400040405     C* Verifico i campi "personalizzati"
098500040405     C*** NSP
098600040405     C*** RMA
098700040119     C*** RMN
098800040405     C*** VATNOT
098900040405     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
099000040119     C                   EVAL      PiStr=SkSplitCSV(i)
099100040119     C                   EXSR      CHKNUM
099200040119     C                   IF        PiInt=*on
099300040405     C                   Z-ADD     PiVal         wRMANOT
099400040405     C                   EVAL      VABRMA = %trim(%editc(wRMANOT:'4'))
099500040405     C                   Z-ADD     PiVal         VABRMN
099600040119     C                   Z-ADD     PiVal         VABNSP
099700040119     C                   Z-ADD     PiVal         VATNSP
099800040405     C                   EVAL      VATNOT = %trim(VABRMA)
099900040119     C                   ELSE
100000040119     C                   SETON                                        32
100100040119     C                   EVAL      VABRMN = 1
100200040119     C                   EVAL      vinmsg = %trimr(vinmsg)
100300040405     C                             + ' ' + 'VABRMN VABRMA VABNSP VATNSP VATNOT'
100400040119     C                   ENDIF
100500040119     C                   ENDIF
100600040119     C*** CBO
100700040405     C                   EVAL      VABCBO = '1'
100800030721     C*
100900030721     C* Reperisco quindi i campi numerici...
101000030721     C*** CAD
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
101200030721     C                   EVAL      PiStr=SkSplitCSV(i)
101300030721     C                   EXSR      CHKNUM
101400030721     C                   IF        PiInt=*on
101500030721     C                   Z-ADD     PiVal         Num5_0
101600030721     C                   MOVEL(p)  Num5_0        VABCAD
101700030721     C                   ELSE
101800030721     C                   SETON                                        32
101900030721     C                   EVAL      VABCAD = *zeros
102000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102100030721     C                             + ' ' + 'VABCAD'
102200030721     C                   ENDIF
102300030721     C                   ENDIF
102400030721     C*** NCL
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
102600030721     C                   EVAL      PiStr=SkSplitCSV(i)
102700030721     C                   EXSR      CHKNUM
102800030721     C                   IF        PiInt=*on
102900030721     C                   Z-ADD     PiVal         VABNCL
103000030721     C                   ELSE
103100030721     C                   SETON                                        32
103200030721     C                   EVAL      VABNCL = *zeros
103300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103400040405     C                             + ' ' + 'VABNCL'
103500030721     C                   ENDIF
103600030721     C                   ENDIF
103700030721     C*** PKB
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
103900030721     C                   EVAL      PiStr=SkSplitCSV(i)
104000030721     C                   EXSR      CHKNUM
104100030721     C                   IF        PiNum=*on
104200030721     C                   Z-ADD     PiVal         VABPKB
104300030721     C                   ELSE
104400030721     C                   SETON                                        32
104500030721     C                   EVAL      VABPKB = *zeros
104600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104700030721     C                             + ' ' + 'VABPKB'
104800030721     C                   ENDIF
104900030721     C                   ENDIF
105000030721     C*** CAS
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
105200030721     C                   EVAL      PiStr=SkSplitCSV(i)
105300030721     C                   EXSR      CHKNUM
105400030721     C                   IF        PiNum=*on
105500030721     C                   Z-ADD     PiVal         VABCAS
105600030721     C                   ELSE
105700030721     C                   SETON                                        32
105800030721     C                   EVAL      VABCAS = *zeros
105900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106000030721     C                             + ' ' + 'VABCAS'
106100030721     C                   ENDIF
106200030721     C                   ENDIF
106300030721     C*** CCM
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
106500030721     C                   EVAL      PiStr=SkSplitCSV(i)
106600030721     C                   EXSR      CHKNUM
106700030721     C                   IF        PiInt=*on      AND
106800030721     C                             PiVal<=9999999 AND
106900030721     C                             PiVal>*zeros
107000030721     C                   Z-ADD     PiVal         VABCCM
107100040119     C                   Z-ADD     PiVal         VATCCM
107200030721     C                   ELSE
107300030721     C                   SETON                                        32
107400030721     C                   EVAL      VABCCM = *zeros
107500040419     C                   EVAL      VATCCM = *zeros
107600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107700040119     C                             + ' ' + 'VABCCM VATCCM'
107800030721     C                   ENDIF
107900030721     C                   ENDIF
108000030721     C*** LNP
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
108200030721     C                   EVAL      PiStr=SkSplitCSV(i)
108300030721     C                   EXSR      CHKNUM
108400030721     C                   IF        PiInt=*on    AND
108500030721     C                             PiVal<=999   AND
108600030721     C                             PiVal>*zeros
108700030721     C                   Z-ADD     PiVal         VABLNP
108800040119     C                   Z-ADD     PiVal         VATLNP
108900030721     C                   ELSE
109000030721     C                   SETON                                        32
109100030721     C                   EVAL      VABLNP = *zeros
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300040119     C                             + ' ' + 'VABLNP VATLNP'
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
127700030715     C*
127800030715     C                   ENDIF
127900030715     C                   EVAL      i = i + 1
128000030715     C                   ENDDO
128100040113     C*
128200040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
128300040113     C                   IF        VABCAS > *zeros
128400040119     C                   EVAL      VABVCA = 'EUR'
128500040119     C                   IF        VABCBO = '1'
128600040113     C                   EVAL      VABCBO = '4'
128700040119     C                   ENDIF
128800040119     C                   IF        VABCBO = '2'
128900040119     C                   EVAL      VABCBO = '6'
129000040119     C                   ENDIF
129100040113     C                   ENDIF
129200020204     C*
129300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
129400020204     C                   EXSR      CHKIMPDIV
129500030715     C                   ENDIF
129600020204     C*
129700010607     C                   ENDSR
129800010607     C*----------------------------------------------------*
129900010601
130000020204
130100020204     C*----------------------------------------------------*
130200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
130300020204     C*----------------------------------------------------*
130400020204     C     CHKIMPDIV     BEGSR
130500020204     C*
130600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
130700020204     C                   Z-ADD     *zeros        wrkDec            9 9
130800020204     C*
130900020204     C* Come prima cosa effettuo considerazioni sulla divisa
131000020204     C                   IF        vabIAS > *zeros
131100020204     C                   IF        vabVAS <> 'EUR'
131200020204     C                   EVAL      vabVAS =  'ITL'
131300020204     C                   ENDIF
131400020204     C                   ENDIF
131500020204     C*
131600020204     C                   IF        vabCAS > *zeros
131700020204     C                   IF        vabVCA <> 'EUR'
131800020204     C                   EVAL      vabVCA =  'ITL'
131900020204     C                   ENDIF
132000020204     C                   ENDIF
132100020204     C*
132200020204     C                   IF        vabVMD > *zeros
132300020204     C                   IF        vabVAD <> 'EUR'
132400020204     C                   EVAL      vabVAD =  'ITL'
132500020204     C                   ENDIF
132600020204     C                   ENDIF
132700020204     C*
132800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
132900020204     C                   Z-ADD     vabIAS        wrkDec
133000020204     C                   IF        wrkDec > *zeros
133100020204     C                   IF        vabVAS = 'ITL'
133200020204     C                   EVAL      vabIAS = *zeros
133300020204     C                   ENDIF
133400020204     C                   ENDIF
133500020204     C*
133600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
133700020204     C                   Z-ADD     vabCAS        wrkDec
133800020204     C                   IF        wrkDec > *zeros
133900020204     C                   IF        vabVCA = 'ITL'
134000020204     C                   EVAL      vabCAS = *zeros
134100020204     C                   ENDIF
134200020204     C                   ENDIF
134300020204     C*
134400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
134500020204     C                   Z-ADD     vabVMD        wrkDec
134600020204     C                   IF        wrkDec > *zeros
134700020204     C                   IF        vabVAD = 'ITL'
134800020204     C                   EVAL      vabVMD = *zeros
134900020204     C                   ENDIF
135000020204     C                   ENDIF
135100020204     C*
135200020204     C                   ENDSR
135300020204     C***
135400020204
135500010330
135600010330
135700010330     C*----------------------------------------------------*
135800010330     C*  CONTROLLO NUMERICITA' CAMPI
135900010330     C*----------------------------------------------------*
136000010330     C     CHKNUM        BEGSR
136100010330     C*
136200010606     C                   IF        PiDecChr = *blanks
136300030715     C                   EVAL      PiDecChr = CharNUM
136400010606     C                   ENDIF
136500010606     C*
136600010606     C                   CALL(e)   'ISNUMERIC'
136700010330     C                   PARM                    PiStr            30
136800010606     C                   PARM                    PiDecChr          1
136900010330     C                   PARM      *ZEROS        PiVal            30 9
137000010330     C                   PARM      '0'           PiInt             1
137100010330     C                   PARM      '0'           PiNum             1
137200010330     C                   IF        %error
137300010606     C                   EVAL      PiNum=*off
137400010330     C                   ENDIF
137500010330     C*
137600010330     C                   ENDSR
137700010330     C***
137800010330
137900010601
138000010601
138100010601
138200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
138300010601     C     repfil        BEGSR
138400010601     C*
138500010601     C                   if        invfil = *zeros and
138600010601     C                             depfil > *zeros and
138700010629     C                             (vinflg = *blanks or
138800010629     C                              vinflg = *zeros)
138900010601     C
139000010601     C                   eval      invfil = depfil
139100010601     C                   endif
139200010601     C*
139300010601     C                   if        depfil <> invfil and
139400010601     C                             invfil > *zeros
139500010601     C                   eval      flgMulti = '1'
139600010601     C                   if        vinflg = *blanks
139700010601     C                   add       1             cntNonEl
139800010601     C                   endif
139900010601     C                   endif
140000010601     C*
140100010601     C                   if        vinflg = '2'
140200010601     C                   eval      flgStato = '2'
140300010601     C                   endif
140400010601     C*
140500010601     C                   ENDSR
140600010601     C***
140700010601
140800010601
140900010601
141000010330
141100040119      /TITLE Invio dei dati al punto operativo.
141200040119     C     invio         BEGSR
141300040119     C*
141400040119     C* 1° invio FIVAT
141500040119     C                   reset                   dscmz
141600040119     C                   move      vlrpoi        cmzdst
141700040119     C                   eval      cmzfld = 'FIVATWWR'
141800040119     C                   eval      cmzmbd = vlrhdl
141900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
142000040119     C***                if        prmfir = *blanks
142100040119     C                   eval      cmzfla = 'FIVAT00F'
142200040119     C                   eval      cmzmba = 'FIVAT00F'
142300040119     C***                else
142400040119     C***                eval      cmzfla = prmfir
142500040119     C***                eval      cmzmba = prmfir
142600040119     C***                endif
142700040119     C                   eval      cmznrr = *zeros
142800040119     C                   move      §ctrokvt      cmznrr
142900040119     C                   eval      cmzlba = vlrfl1
143000040119     C                   call(e)   'TIS711C'
143100040119     C                   parm                    dscmz
143200040119     C                   parm      *blanks       esito
143300040119     C                   if        %error
143400040119     C                             or cmzerr = '1'
143500040119     C                             or esito  = '1'
143600040119     C                   eval      wrkesito = '3'
143700040119     C                   else
143800040119     C*
143900040119     C* 2° invio FIVAB
144000040119     C                   reset                   dscmz
144100040119     C                   move      vlrpoi        cmzdst
144200040119     C                   eval      cmzfld = vlrfou
144300040119     C                   eval      cmzmbd = vlrhdl
144400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
144500040119     C***                if        prmfir = *blanks
144600040119     C                   eval      cmzfla = 'FIVAB00F'
144700040119     C                   eval      cmzmba = 'FIVAB00F'
144800040119     C***                else
144900040119     C***                eval      cmzfla = prmfir
145000040119     C***                eval      cmzmba = prmfir
145100040119     C***                endif
145200040119     C                   eval      cmznrr = *zeros
145300040119     C                   move      §ctrokvb      cmznrr
145400040119     C                   eval      cmzlba = vlrfl1
145500040119     C                   call(e)   'TIS711C'
145600040119     C                   parm                    dscmz
145700040119     C                   parm      *blanks       esito
145800040119     C                   if        %error
145900040119     C                             or cmzerr = '1'
146000040119     C                             or esito  = '1'
146100040119     C                   eval      wrkesito = '3'
146200040119     C                   endif
146300040119     C                   endif
146400040119     C*
146500040119     C                   ENDSR
146600040119     C***
146700010601
146800010601
146900010601
147000010601
147100010601      /TITLE Invio dei dati al punto operativo.
147200010601     C     opeini        BEGSR
147300010601     C*
147400010601     C* Inizializzo flag e contatori operativi
147500010601     C                   movel     '0'           flgGiro           1
147600010601     C                   movel     '0'           flgMulti          1
147700010601     C                   movel     '1'           flgStato          1
147800010615     C                   movel     '0'           flgOk             1
147900010601     C                   z-add     *zeros        cntNonEl         10 0
148000010601     C                   z-add     *zeros        depfil            3 0
148100010601     C                   z-add     *zeros        invfil            3 0
148200010601     C*
148300010601     C                   ENDSR
148400010601     C***
148500010601
148600010601
148700050421
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
151200050421
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
153700990908
153800040405     Otitva9p   E            riepilogo         2
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
156300040405     Otitva9ps  E            testdett          2
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
159400040405OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
159500040405OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
159600040405DLTOVR FILE(TITVA9P TITVA9PS) LVL(*)
159700010606
159800000613
159900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
160000040405VABRSD;VABRD2;VABIND;VABCAD;VABLOD;
160100040405VABPRD;VABNCL;VABPKB;VABNSP;VABNOT;VABNT2;DATA
160200040113
