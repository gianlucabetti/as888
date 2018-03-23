000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800061211     Ftitvv8p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100061211     Ftitvv8ps  O    f  198        PRINTER usropn
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
013100061211     C                   if        not %open(titvv8ps)
013200061211     C                   open      titvv8ps
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
015600050201     C*
015700050201     C* Effettuo lancio TISI95 solo x chiusura
015800050201     C                   CLEAR                   TISI95DS
015900050201     C                   EVAL      I95TLA = 'C'
016000050201     C                   CALL      'TISI95R'
016100050201     C                   PARM                    TISI95DS
016200000613     C*
016300061211     C                   if        %open(titvv8ps)
016400000616     C                   except    findett
016500061211     C                   close     titvv8ps
016600000613     C                   endif
016700000616     C*
016800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016900011113     C                   Z-ADD     150           LENGH            15 5
017000010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017100000616     C                   PARM                    CMD(3)
017200000616     C                   PARM                    LENGH
017300000616     C*
017400010601     C                   ENDSR
017500010601     C***
017600010601
017700010601
017800010601
017900000613
018000000613     C*--------------------------------------------------------
018100000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018200000613     C*--------------------------------------------------------
018300000621     C     STPR          BEGSR
018400000613     C*
018500061211     C                   if        not %open(titvv8p)
018600061211     C   50              open      titvv8p
018700990915     C                   endif
018800990915     C*
018900030715     C   50              except    riepilogo
019000990915     C*
019100061211     C                   if        %open(titvv8p)
019200061211     C   50              close     titvv8p
019300990914     C                   endif
019400000613     C*
019500000613     C                   ENDSR
019600000613     C***
019700000613
019800010601
019900010601
020000010330     C*--------------------------------------------------------
020100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020200010330     C*--------------------------------------------------------
020300010605     C     RWVAB         BEGSR
020400010330     C*
020500010330     C                   if        not %open(tivin00r)
020600010330     C                   open      tivin00r
020700010330     C                   endif
020800021025     C                   if        not %open(fivabwwr)
020900021025     C                   open      fivabwwr
021000010330     C                   endif
021100040119     C*
021200040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021300040119     C                   exsr      prevat
021400040119     C*
021500040119     C                   if        chkcall = '0'
021600040119     C*
021700040119     C                   if        not %open(fivatwwr)
021800040119     C                   open      fivatwwr
021900040119     C                   endif
022000010330     C*
022100040119     C                   clear                   §CTROKVB          7 0
022200040119     C                   clear                   §CTROKVT          7 0
022300040119     C                   clear                   §CTRMO            7 0
022400040119     C                   clear                   §CTRNO            7 0
022500040719     C*
022600040719     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
022700040719     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
022800040719     C                   exsr      inzvar
022900040719     C                   exsr      impvab
023000040706     C*
023100040706     C  N70              do        *HIVAL
023200010330     C*
023300040706     C                   read      tivin00r                               70
023400010618     C*
023500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023600010618     C                   if        vinflg = '1'
023700010618     C                   eval      flgOk = '1'
023800010618     C                   endif
023900010618     C*
024000040316     C                   if        (vindta > *blanks AND
024100040316     C                              %trim(vindta) <> *all';') OR
024200040316     C                             wGiro = *zeros
024300010330     C                   add       1             rrnum
024400010330     C*
024500010601     C                   if        *in70 = *off and
024600010330     C                             (vinflg = *blanks
024700010330     C                              or vinflg = '0'
024800010330     C                              or vinflg = '2')
024900010330     C*
025000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025100010711     C                   if        vinflg = *blanks or vinflg = '0'
025200010711     C                   clear                   vinmsg
025300010711     C                   endif
025400010601     C*
025500010330     C                   exsr      inzvar
025600010605     C                   exsr      impvab
025700010601     C*
025800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025900010605     C                   eval      depfil = VABLNP
026000010601     C                   exsr      repfil
026100010601     C                   if        depfil = invfil
026200021025     C                   if        vlrpoi = 999
026300030822     C                   move(P)   invfil        VABFGS
026400040119     C                   move(P)   invfil        VATFGS
026500021025     C                   else
026600030822     C                   move(P)   vlrpoi        VABFGS
026700040119     C                   move(P)   vlrpoi        VATFGS
026800021025     C                   endif
026900010601     C*
027000010601     C                   exsr      PREELA
027100010601     C*
027200010604     C* Ebbene...
027300010604     C*
027400040119     C  N31              add       1             §CTROKVB
027500040119     C   32              add       1             §CTRMO
027600040119     C   31              add       1             §CTRNO
027700030822     C                   if        wGiro = 2
027800040119     C                   if        depspe =  *zeros OR
027900040119     C                             depspe <> vabNSP
028000040119     C  N31              exsr      wrivab
028100040119     C  N31              eval      depspe = vabNSP
028200040119     C                   endif
028300040119     C                   endif
028400010604     C*
028500010604     C                   if        *in31 = *off and
028600010604     C                             *in32 = *off
028700010604     C                   eval      vinflg = '1'
028800010604     C                   else
028900010604     C                   eval      recko = vindta
029000010604     C                   except    rigadett
029100010604     C                   eval      vinflg = '2'
029200010604     C                   endif
029300010604     C                   endif
029400010601     C*
029500010604     C                   endif
029600010604     C*
029700010330     C                   else
029800010330     C                   eval      vinflg = '1'
029900010330     C                   endif
030000010601     C*
030100010601     C  N70              update    tivin000
030200010330     C*
030300030822     C  N70              enddo
030400040119     C*
030500040119     C                   endif                                                  (endif - chkcall)
030600010601     C*
030700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030800010601     C                   if        cntNonEl = *zeros or
030900010601     C                             flgMulti = '0'
031000010330     C* Se non ci sono record con errori ...
031100010601     C                   if        §ctrno = 0 and
031200010604     C                             §ctrmo = 0 and
031300010601     C                             flgStato <> '2'
031400010330     C* ... restituisco esito OK.
031500010330     C                   eval      wrkesito = '0'
031600010330     C                   else
031700040119     C                   if        §ctrokvb > 0
031800010330     C                   eval      wrkesito = '1'
031900010330     C                   else
032000010615     C                   if        flgOk = '0'
032100010615     C                   eval      wrkesito = '2'
032200010615     C                   else
032300010615     C                   eval      wrkesito = '6'
032400010615     C                   endif
032500010330     C                   endif
032600010330     C                   endif
032700010601     C                   else
032800010601     C                   eval      wrkesito = '9'
032900010601     C                   endif
033000010330     C*
033100010330     C                   if        %open(tivin00r)
033200010330     C                   close     tivin00r
033300010330     C                   endif
033400021025     C                   if        %open(fivabwwr)
033500021025     C                   close     fivabwwr
033600010330     C                   endif
033700040512     C                   if        %open(fivatwwr)
033800040512     C                   close     fivatwwr
033900040512     C                   endif
034000010601     C*
034100010601     C                   if        vlrpoi <> 999
034200010601     C                   eval      invfil = vlrpoi
034300010601     C                   endif
034400010330     C*
034500040119     C                   if        §ctrokvb > 0
034600010601     C                             and invfil > *zeros
034700010330     C                   exsr      invio
034800010330     C                   endif
034900010601     C*
035000010618     C                   if        flgGiro = '1'
035100010601     C                   exsr      endela
035200010618     C                   endif
035300010330     C*
035400010330     C                   ENDSR
035500010330     C***
035600040119
035700040119
035800040119
035900040119     C*----------------------------------------------------*
036000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036100040119     C*----------------------------------------------------*
036200040119     C     WRIVAB        BEGSR
036300040119     C*
036400040119     C                   write     fivab000                                     => scarico il VAB
036500040119     C*
036600040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
036700040119     C                   exsr      wrivat
036800040119     C*
036900040119     C                   ENDSR
037000040119
037100040119
037200040119
037300040119     C*----------------------------------------------------*
037400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037500040119     C*----------------------------------------------------*
037600040706     C     WRIVAT        BEGSR
037700040706     C*
037800040719     C* Attribuisco la stessa chiave della testata bolle
037900040719     C                   EVAL      VATAAS = VABAAS
038000040719     C                   EVAL      VATLNP = VABLNP
038100040719     C                   EVAL      VATNSP = VABNSP
038200040719     C*
038300061211     C* Imposto il tipo record sul file VAT a 'E' x indicare il "chi sono"
038400061211     C                   EVAL      VATTRC = 'E'
038500040719     C*
038600040719     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
038700040719     C                   IF        VATNOT <> *blanks
038800040706     C                   WRITE     FIVAT000
038900040706     C                   ADD       1             §CTROKVT
039000040719     C                   ENDIF
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
040400061211     C                   eval      parccm = vlrksc
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
042900030822     C*
043000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
043100030822     C                   CLEAR                   FIVAB000
043200040119     C                   CLEAR                   FIVAT000
043300030822     C                   CLEAR                   SkSplitCSV
043400030822     C                   EVAL      i = 1
043500030822     C                   EVAL      posDa = *zeros
043600030822     C                   EVAL      posA  = *zeros
043700030822     C*
043800030822     C* Reimposto i valori di default
043900030822     C                   EXSR      DEFCAM
044000030822     C*
044100010330     C                   ENDSR
044200010330     C*----------------------------------------------------*
044300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
044400010330     C*----------------------------------------------------*
044500010330     C     DEFCAM        BEGSR
044600040119     C*
044700040119     C* Imposto i valori d default a programma dei campi fondamentali
044800061211     C                   EVAL      VABCCM = 0012943
044900061211     C                   EVAL      VATCCM = 0012943
045000040719     C                   EVAL      VABLNP = 001
045100040719     C                   EVAL      VATLNP = 001
045200061211     C                   EVAL      VABCTR = 300
045300080124     C                   EVAL      VABCBO = '1'
045400061211     C                   EVAL      VABCTM = '7Q'
045500061211     C                   EVAL      VABVAS = 'EUR'
045600061211     C                   EVAL      VABVCA = 'EUR'
045700030822     C*
045800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
045900030715     C* e delimitatore testo.
046000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
046100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
046200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
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
073200010607     C*
073300030715     C**********
073400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073500030715     C**********
073600030715     C                   IF        wGiro = *zeros
073700040706     C*
073800040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
073900040706     C                   EVAL      depvinDTA = vinDTA
074000040113     C*
074100040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
074200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
074300040113     C                                      %trim(intcol(3))
074400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
074500031201     C     minu:maiu     XLATE     vindta        vindta
074600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074700030715     C                   DOW       posDa <= %len(%trim(vindta))
074800030715     C*
074900030715     C* Gestisco il 1° campo
075000030715     C                   IF        i = 1
075100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075200030715     C                   EVAL      posDa = 1
075300030715     C* Eseguo lo scan x trovare la fine del primo campo
075400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075500030715     C* A questo punto "estraggo" il campo corrente
075600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075700030715     C                                                    (posA-posDa))
075800030715     C* X i campi successivi al 1°
075900030715     C                   ELSE
076000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076100030715     C                   EVAL      posDa = posA + 1
076200030715     C* Eseguo lo scan x trovare la fine del campo corrente
076300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076400030715     C* Gestisco l'ultimo campo
076500030715     C                   IF        posA = *zeros
076600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
076700030715     C* A questo punto "estraggo" il campo corrente
076800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076900030715     C                                                    (posA-posDa))
077000030715     C                   LEAVE
077100030715     C                   ELSE
077200030715     C* A questo punto "estraggo" il campo corrente
077300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077400030715     C                                                    (posA-posDa))
077500030715     C                   ENDIF
077600030715     C                   ENDIF
077700030715     C* Incremento il contatore d campo
077800030715     C                   EVAL      i = i +1
077900030715     C                   ENDDO
078000030715     C*
078100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078200030715     C* delimitazione testo
078300030715     C                   EVAL      i = 1
078400030715     C                   DOW       i < %elem(SkSplitFLD)
078500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
078600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
078700030715     C                   EVAL      i = i + 1
078800030715     C                   ENDDO
078900030715     C                   EVAL      wGiro = 1
079000040706     C* Ripristino la 1° riga originale
079100040706     C                   EVAL      vinDTA = depvinDTA
079200030715     C                   ELSE
079300030715     C                   EVAL      wGiro = 2
079400031204     C**********
079500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
079600031204     C* d separatore campo
079700031204     C**********
079800031204     C                   EVAL      vindta = %trim(vindta)
079900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
080000031204     C                   EVAL      vindta = %subst(vindta:2)
080100031204     C                   ENDDO
080200031204     C*
080300031204     C                   Z-ADD     *zeros        lunghInput        4 0
080400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
080600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
080700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080800031204     C                   ENDDO
080900030715     C**********
081000030715     C* Effettuo lo split del campo dati d input
081100030715     C**********
081200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081300030715     C                   DOW       posDa <= %len(%trim(vindta))
081400030715     C*
081500030715     C* Gestisco il 1° campo
081600030715     C                   IF        i = 1
081700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081800030715     C                   EVAL      posDa = 1
081900030715     C* Eseguo lo scan x trovare la fine del primo campo
082000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082100030715     C* A questo punto "estraggo" il campo corrente
082200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082300030715     C                                                    (posA-posDa))
082400030715     C* X i campi successivi al 1°
082500030715     C                   ELSE
082600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082700030715     C                   EVAL      posDa = posA + 1
082800031204     C* Verifico che nn vi sia il campo nn valorizzato
082900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
083000031204     C* Se campo nn valorizzato skippo al prossimo
083100031204     C                   EVAL      SkSplitCSV(i) = *blanks
083200031204     C                   EVAL      posA  = posA + 1
083300031204     C                   ELSE
083400030715     C* Eseguo lo scan x trovare la fine del campo corrente
083500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083600030715     C* Gestisco l'ultimo campo
083700030715     C                   IF        posA = *zeros
083800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083900030715     C* A questo punto "estraggo" il campo corrente
084000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084100030715     C                                                    (posA-posDa))
084200030715     C                   LEAVE
084300030715     C                   ELSE
084400030715     C* A questo punto "estraggo" il campo corrente
084500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084600030715     C                                                    (posA-posDa))
084700030715     C                   ENDIF
084800031204     C                   ENDIF
084900030715     C                   ENDIF
085000030715     C* Incremento il contatore d campo
085100030715     C                   EVAL      i = i +1
085200030715     C                   ENDDO
085300030715     C*
085400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085500030715     C* delimitazione testo
085600030715     C                   EVAL      i = 1
085700030715     C                   DOW       i < %elem(SkSplitCSV)
085800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
085900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
086000030715     C                   EVAL      i = i + 1
086100030715     C                   ENDDO
086200030715     C*
086300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086400030715     C                   EVAL      i = 1
086500030715     C                   DOW       i < %elem(SkSplitFLD)
086600030715     C*
086700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
086800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
086900030715     C                             SkSplitCSV(i) <> *blanks
087000030721     C*** ATB
087100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
087200030721     C                   EVAL      VABATB = SkSplitCSV(i)
087300030721     C                   ENDIF
087400030715     C*** RSD
087500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
087600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
087700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
087800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
087900030715     C* ==
088000030715     C                   ENDIF
088100030721     C*** RD2
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
088300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030715     C*** IND
088600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
088700030715     C                   EVAL      VABIND = SkSplitCSV(i)
088800030715     C                   ENDIF
088900030715     C*** LOD
089000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
089100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
089200030715     C                   ENDIF
089300030715     C*** PRD
089400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
089500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
089600030715     C                   ENDIF
089700030721     C*** NZD
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
089900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
090000030721     C                   ENDIF
090100030715     C*** RMA
090200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
090300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
090400030715     C                   ENDIF
090500030715     C*** NOT
090600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
090700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
090800030715     C                   ENDIF
090900030715     C*** NT2
091000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
091100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
091200030715     C                   ENDIF
091300030715     C*** VCA
091400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
091500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
091600030715     C                   ENDIF
091700030715     C*** VAS
091800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
091900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
092000030715     C                   ENDIF
092100030715     C*** FFD
092200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
092300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
092400030715     C                   ENDIF
092500030721     C*** GC1
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
092700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030721     C*** GC2
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
093100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
093200030721     C                   ENDIF
093300030721     C*** TSP
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
093500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
093600030721     C                   ENDIF
093700030721     C*** NAS
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
093900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030721     C*** TIC
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
094300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030721     C*** GCA
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
094700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030721     C*** XCO
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
095100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
095200030721     C                   ENDIF
095300030721     C*** CTM
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
095500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030721     C*** TCR
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
095900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030721     C*** CTS
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
096300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030721     C*** FTM
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
096700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030721     C*** VAD
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
097100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
097200030721     C                   ENDIF
097300030721     C*** GMA
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
097500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030721     C*** GGA
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
097900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030721     C*** GVA
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
098300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** TC1
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
098700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** TC2
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
099100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** SCL
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
099500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** RMO
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
099900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** NMO
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
100300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
100400030721     C                   ENDIF
100500040719     C*** CBO
100600040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100700061211     C                   EVAL      VABCBO = SkSplitCSV(i)
100800040719     C                   ENDIF
100900030721     C*
101000040706     C* Reperisco quindi i campi numerici...
101100040706     C*** NSP
101200040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
101300040706     C                   EVAL      PiStr=SkSplitCSV(i)
101400040706     C                   EXSR      CHKNUM
101500040706     C                   IF        PiInt=*on
101600040706     C                   Z-ADD     PiVal         VABNSP
101700040706     C                   ELSE
101800040706     C                   SETON                                        31
101900040706     C                   EVAL      VABNSP = *zeros
102000040706     C                   EVAL      vinmsg = %trimr(vinmsg)
102100040706     C                             + ' ' + 'VABNSP'
102200040706     C                   ENDIF
102300040706     C                   ENDIF
102400040706     C*** RMN
102500040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
102600040706     C                   EVAL      PiStr=SkSplitCSV(i)
102700040706     C                   EXSR      CHKNUM
102800040706     C                   IF        PiInt=*on
102900040706     C                   Z-ADD     PiVal         VABRMN
103000040706     C                   ELSE
103100040706     C                   SETON                                        32
103200040706     C                   EVAL      VABRMN = 1
103300040706     C                   EVAL      vinmsg = %trimr(vinmsg)
103400040706     C                             + ' ' + 'VABRMN'
103500040706     C                   ENDIF
103600040706     C                   ENDIF
103700030721     C*** CAD
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
103900030721     C                   EVAL      PiStr=SkSplitCSV(i)
104000030721     C                   EXSR      CHKNUM
104100030721     C                   IF        PiInt=*on
104200030721     C                   Z-ADD     PiVal         Num5_0
104300030721     C                   MOVEL(p)  Num5_0        VABCAD
104400030721     C                   ELSE
104500030721     C                   SETON                                        32
104600030721     C                   EVAL      VABCAD = *zeros
104700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104800030721     C                             + ' ' + 'VABCAD'
104900030721     C                   ENDIF
105000030721     C                   ENDIF
105100030721     C*** NCL
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
105300030721     C                   EVAL      PiStr=SkSplitCSV(i)
105400030721     C                   EXSR      CHKNUM
105500030721     C                   IF        PiInt=*on
105600030721     C                   Z-ADD     PiVal         VABNCL
105700030721     C                   ELSE
105800030721     C                   SETON                                        32
105900030721     C                   EVAL      VABNCL = *zeros
106000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106100040706     C                             + ' ' + 'VABNCL'
106200030721     C                   ENDIF
106300030721     C                   ENDIF
106400030721     C*** PKB
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
106600030721     C                   EVAL      PiStr=SkSplitCSV(i)
106700030721     C                   EXSR      CHKNUM
106800030721     C                   IF        PiNum=*on
106900030721     C                   Z-ADD     PiVal         VABPKB
107000030721     C                   ELSE
107100030721     C                   SETON                                        32
107200030721     C                   EVAL      VABPKB = *zeros
107300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107400030721     C                             + ' ' + 'VABPKB'
107500030721     C                   ENDIF
107600030721     C                   ENDIF
107700030721     C*** CAS
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
107900030721     C                   EVAL      PiStr=SkSplitCSV(i)
108000030721     C                   EXSR      CHKNUM
108100030721     C                   IF        PiNum=*on
108200030721     C                   Z-ADD     PiVal         VABCAS
108300030721     C                   ELSE
108400030721     C                   SETON                                        32
108500030721     C                   EVAL      VABCAS = *zeros
108600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108700030721     C                             + ' ' + 'VABCAS'
108800030721     C                   ENDIF
108900030721     C                   ENDIF
109000030721     C*** CCM
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
109200030721     C                   EVAL      PiStr=SkSplitCSV(i)
109300030721     C                   EXSR      CHKNUM
109400030721     C                   IF        PiInt=*on      AND
109500030721     C                             PiVal<=9999999 AND
109600030721     C                             PiVal>*zeros
109700030721     C                   Z-ADD     PiVal         VABCCM
109800040119     C                   Z-ADD     PiVal         VATCCM
109900030721     C                   ELSE
110000030721     C                   SETON                                        32
110100030721     C                   EVAL      VABCCM = *zeros
110200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110300040119     C                             + ' ' + 'VABCCM VATCCM'
110400030721     C                   ENDIF
110500030721     C                   ENDIF
110600030721     C*** LNP
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
110800030721     C                   EVAL      PiStr=SkSplitCSV(i)
110900030721     C                   EXSR      CHKNUM
111000030721     C                   IF        PiInt=*on    AND
111100030721     C                             PiVal<=999   AND
111200030721     C                             PiVal>*zeros
111300030721     C                   Z-ADD     PiVal         VABLNP
111400040119     C                   Z-ADD     PiVal         VATLNP
111500030721     C                   ELSE
111600030721     C                   SETON                                        32
111700030721     C                   EVAL      VABLNP = *zeros
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900040119     C                             + ' ' + 'VABLNP VATLNP'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** NRS
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on AND
112700030721     C                             PiVal<=99
112800030721     C                   Z-ADD     PiVal         VABNRS
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABNRS'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** LNA
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on    AND
114000030721     C                             PiVal<=999   AND
114100030721     C                             PiVal>*zeros
114200030721     C                   Z-ADD     PiVal         VABLNA
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      VABLNA = *zeros
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700030721     C                             + ' ' + 'VABLNA'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** CTR
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiInt=*on  AND
115500030721     C                             PiVal<=999
115600030721     C                   Z-ADD     PiVal         VABCTR
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116000030721     C                             + ' ' + 'VABCTR'
116100030721     C                   ENDIF
116200030721     C                   ENDIF
116300030721     C*** IAS
116400080207     C**!!!              IF        %trim(SkSplitFLD(i)) = 'VABIAS'
116500080207     C**!!!              EVAL      PiStr=SkSplitCSV(i)
116600080207     C**!!!              EXSR      CHKNUM
116700080207     C**!!!              IF        PiNum=*on
116800080207     C**!!!              Z-ADD     PiVal         VABIAS
116900080207     C**!!!              ELSE
117000080207     C**!!!              SETON                                        32
117100080207     C**!!!              EVAL      vinmsg = %trimr(vinmsg)
117200080207     C**!!!                        + ' ' + 'VABIAS'
117300080207     C**!!!              ENDIF
117400080207     C**!!!              ENDIF
117500030721     C*** VLB
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900030721     C                   IF        PiNum=*on
118000030721     C                   Z-ADD     PiVal         VABVLB
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABVLB'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** QFT
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiNum=*on
119200030721     C                   Z-ADD     PiVal         VABQFT
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABQFT'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** NCD
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on      AND
120400030721     C                             PiVal<=9999999
120500030721     C                   Z-ADD     PiVal         VABNCD
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABNCD'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** NCA
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on      AND
121700030721     C                             PiVal<=9999999
121800030721     C                   Z-ADD     PiVal         VABNCA
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200030721     C                             + ' ' + 'VABNCA'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** ZNC
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiInt=*on AND
123000030721     C                             PiVal<=99
123100030721     C                   Z-ADD     PiVal         VABZNC
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABZNC'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** DCR
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on       AND
124300030721     C                             PiVal<=99999999
124400030721     C                   Z-ADD     PiVal         VABDCR
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABDCR'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** HCR
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiInt=*on   AND
125600030721     C                             PiVal<=9999
125700030721     C                   Z-ADD     PiVal         VABHCR
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABHCR'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** VMD
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiNum=*on
126900030721     C                   Z-ADD     PiVal         VABVMD
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABVMD'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** ANT
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on        AND
128100030721     C                             PiVal<=999999999
128200030721     C                   Z-ADD     PiVal         VABANT
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABANT'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** CMO
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiInt=*on
129400030721     C                   Z-ADD     PiVal         Num5_0
129500030721     C                   MOVEL(p)  Num5_0        VABCMO
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800030721     C                   EVAL      VABCMO = *zeros
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABCMO'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300040706     C*
130400040719     C*** VATNOT
130500040719     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
130600040719     C                   EVAL      VATNOT = SkSplitCSV(i)
130700040719     C                   ENDIF
130800080131     C*
130900080131     C*******
131000080131     C* Forzatura particolari dentro ciclo
131100080131     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1GC1'
131200080131     C                   IF        SkSplitCSV(i) = 'A'
131300080131     C                   EVAL      VABTC1 = 'A'
131400080131     C                   ENDIF
131500080328     C                   IF        SkSplitCSV(i) = 'S'
131600080328     C                   EVAL      VABFFD = 'S'
131700080328     C                   ENDIF
131800080131     C                   IF        SkSplitCSV(i) = 'M'
131900080131     C                   EVAL      %subst(VABGC1:2:1) = 'P'
132000080131     C                   ENDIF
132100080131     C                   IF        SkSplitCSV(i) = 'P'
132200080131     C                   EVAL      %subst(VABGC1:2:1) = 'M'
132300080131     C                   ENDIF
132400080131     C                   ENDIF
132500080131     C*******
132600030715     C*
132700030715     C                   ENDIF
132800030715     C                   EVAL      i = i + 1
132900030715     C                   ENDDO
133000040706     C*
133100040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
133200040706     C* Reperisco la provincia dal CAP e dalla località
133300040706     C                   IF        VABCAD <> *blanks
133400040706     C                   CLEAR                   TISI95DS
133500040706     C                   EVAL      I95TCN = '3'
133600040706     C                   Z-ADD     datcor        I95DAT
133700040706     C                   EVAL      I95CAP = VABCAD
133800040706     C                   EVAL      I95LOC = VABLOD
133900040706     C                   CALL      'TISI95R'
134000040706     C                   PARM                    TISI95DS
134100040706     C                   EVAL      VABPRD = O95PRV
134200040706     C                   ENDIF
134300061211     C*******
134400061211     C* Forzatura particolare: il numero spedizione bolla corrisponde al "chi sono"
134500061211     C* (ovviamente sono tutte spedizioni mono collo)
134600061211     C                   EVAL      PiStr=VATNOT
134700061211     C                   EXSR      CHKNUM
134800061211     C                   IF        PiNum=*on
134900061211     C                   Z-ADD     PiVal         VABNSP
135000061211     C                   ENDIF
135100061211     C*******
135200040719     C*
135300040719     C* Se NSP nn valorizzato considero errore bloccante in traduzione
135400040719     C                   IF        VABNSP = *zeros
135500040719     C                   SETON                                        31
135600040719     C                   ENDIF
135700040719     C*
135800040719     C* Se LNP nn valorizzato considero errore bloccante in traduzione
135900040719     C                   IF        VABLNP = *zeros
136000040719     C                   SETON                                        31
136100040719     C                   ENDIF
136200040706     C*
136300040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
136400040706     C* d valorizzarli sempre entrambi
136500040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
136600040706     C                   Z-ADD     VABNSP        VABRMN
136700040706     C                   ENDIF
136800040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
136900040706     C                   MOVEL     VABRMN        VABRMA
137000040706     C                   ENDIF
137100040113     C*
137200040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
137300040113     C                   IF        VABCAS > *zeros
137400110124     C***                EVAL      VABTIC = 'BM'
137500110124     C                   EVAL      VABTIC = *blanks
137600040119     C                   EVAL      VABVCA = 'EUR'
137700040119     C                   IF        VABCBO = '1'
137800040113     C                   EVAL      VABCBO = '4'
137900040119     C                   ENDIF
138000040119     C                   IF        VABCBO = '2'
138100040119     C                   EVAL      VABCBO = '6'
138200040119     C                   ENDIF
138300040113     C                   ENDIF
138400020204     C*
138500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
138600020204     C                   EXSR      CHKIMPDIV
138700030715     C                   ENDIF
138800020204     C*
138900010607     C                   ENDSR
139000010607     C*----------------------------------------------------*
139100010601
139200020204
139300020204     C*----------------------------------------------------*
139400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
139500020204     C*----------------------------------------------------*
139600020204     C     CHKIMPDIV     BEGSR
139700020204     C*
139800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
139900020204     C                   Z-ADD     *zeros        wrkDec            9 9
140000020204     C*
140100020204     C* Come prima cosa effettuo considerazioni sulla divisa
140200020204     C                   IF        vabIAS > *zeros
140300020204     C                   IF        vabVAS <> 'EUR'
140400020204     C                   EVAL      vabVAS =  'ITL'
140500020204     C                   ENDIF
140600020204     C                   ENDIF
140700020204     C*
140800020204     C                   IF        vabCAS > *zeros
140900020204     C                   IF        vabVCA <> 'EUR'
141000020204     C                   EVAL      vabVCA =  'ITL'
141100020204     C                   ENDIF
141200020204     C                   ENDIF
141300020204     C*
141400020204     C                   IF        vabVMD > *zeros
141500020204     C                   IF        vabVAD <> 'EUR'
141600020204     C                   EVAL      vabVAD =  'ITL'
141700020204     C                   ENDIF
141800020204     C                   ENDIF
141900020204     C*
142000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
142100020204     C                   Z-ADD     vabIAS        wrkDec
142200020204     C                   IF        wrkDec > *zeros
142300020204     C                   IF        vabVAS = 'ITL'
142400020204     C                   EVAL      vabIAS = *zeros
142500020204     C                   ENDIF
142600020204     C                   ENDIF
142700020204     C*
142800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
142900020204     C                   Z-ADD     vabCAS        wrkDec
143000020204     C                   IF        wrkDec > *zeros
143100020204     C                   IF        vabVCA = 'ITL'
143200020204     C                   EVAL      vabCAS = *zeros
143300020204     C                   ENDIF
143400020204     C                   ENDIF
143500020204     C*
143600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
143700020204     C                   Z-ADD     vabVMD        wrkDec
143800020204     C                   IF        wrkDec > *zeros
143900020204     C                   IF        vabVAD = 'ITL'
144000020204     C                   EVAL      vabVMD = *zeros
144100020204     C                   ENDIF
144200020204     C                   ENDIF
144300020204     C*
144400020204     C                   ENDSR
144500020204     C***
144600020204
144700010330
144800010330
144900010330     C*----------------------------------------------------*
145000010330     C*  CONTROLLO NUMERICITA' CAMPI
145100010330     C*----------------------------------------------------*
145200010330     C     CHKNUM        BEGSR
145300010330     C*
145400010606     C                   IF        PiDecChr = *blanks
145500030715     C                   EVAL      PiDecChr = CharNUM
145600010606     C                   ENDIF
145700010606     C*
145800010606     C                   CALL(e)   'ISNUMERIC'
145900010330     C                   PARM                    PiStr            30
146000010606     C                   PARM                    PiDecChr          1
146100010330     C                   PARM      *ZEROS        PiVal            30 9
146200010330     C                   PARM      '0'           PiInt             1
146300010330     C                   PARM      '0'           PiNum             1
146400010330     C                   IF        %error
146500010606     C                   EVAL      PiNum=*off
146600010330     C                   ENDIF
146700010330     C*
146800010330     C                   ENDSR
146900010330     C***
147000010330
147100010601
147200010601
147300010601
147400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
147500010601     C     repfil        BEGSR
147600010601     C*
147700010601     C                   if        invfil = *zeros and
147800010601     C                             depfil > *zeros and
147900010629     C                             (vinflg = *blanks or
148000010629     C                              vinflg = *zeros)
148100010601     C
148200010601     C                   eval      invfil = depfil
148300010601     C                   endif
148400010601     C*
148500010601     C                   if        depfil <> invfil and
148600010601     C                             invfil > *zeros
148700010601     C                   eval      flgMulti = '1'
148800010601     C                   if        vinflg = *blanks
148900010601     C                   add       1             cntNonEl
149000010601     C                   endif
149100010601     C                   endif
149200010601     C*
149300010601     C                   if        vinflg = '2'
149400010601     C                   eval      flgStato = '2'
149500010601     C                   endif
149600010601     C*
149700010601     C                   ENDSR
149800010601     C***
149900010601
150000010601
150100010601
150200010330
150300040119      /TITLE Invio dei dati al punto operativo.
150400040119     C     invio         BEGSR
150500040119     C*
150600040119     C* 1° invio FIVAT
150700040119     C                   reset                   dscmz
150800040119     C                   move      vlrpoi        cmzdst
150900040119     C                   eval      cmzfld = 'FIVATWWR'
151000040119     C                   eval      cmzmbd = vlrhdl
151100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
151200040119     C***                if        prmfir = *blanks
151300040119     C                   eval      cmzfla = 'FIVAT00F'
151400040119     C                   eval      cmzmba = 'FIVAT00F'
151500040119     C***                else
151600040119     C***                eval      cmzfla = prmfir
151700040119     C***                eval      cmzmba = prmfir
151800040119     C***                endif
151900040119     C                   eval      cmznrr = *zeros
152000040119     C                   move      §ctrokvt      cmznrr
152100040119     C                   eval      cmzlba = vlrfl1
152200040119     C                   call(e)   'TIS711C'
152300040119     C                   parm                    dscmz
152400040119     C                   parm      *blanks       esito
152500040119     C                   if        %error
152600040119     C                             or cmzerr = '1'
152700040119     C                             or esito  = '1'
152800040119     C                   eval      wrkesito = '3'
152900040119     C                   else
153000040119     C*
153100040119     C* 2° invio FIVAB
153200040119     C                   reset                   dscmz
153300040119     C                   move      vlrpoi        cmzdst
153400040119     C                   eval      cmzfld = vlrfou
153500040119     C                   eval      cmzmbd = vlrhdl
153600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
153700040119     C***                if        prmfir = *blanks
153800040119     C                   eval      cmzfla = 'FIVAB00F'
153900040119     C                   eval      cmzmba = 'FIVAB00F'
154000040119     C***                else
154100040119     C***                eval      cmzfla = prmfir
154200040119     C***                eval      cmzmba = prmfir
154300040119     C***                endif
154400040119     C                   eval      cmznrr = *zeros
154500040119     C                   move      §ctrokvb      cmznrr
154600040119     C                   eval      cmzlba = vlrfl1
154700040119     C                   call(e)   'TIS711C'
154800040119     C                   parm                    dscmz
154900040119     C                   parm      *blanks       esito
155000040119     C                   if        %error
155100040119     C                             or cmzerr = '1'
155200040119     C                             or esito  = '1'
155300040119     C                   eval      wrkesito = '3'
155400040119     C                   endif
155500040119     C                   endif
155600040119     C*
155700040119     C                   ENDSR
155800040119     C***
155900010601
156000010601
156100010601
156200010601
156300010601      /TITLE Invio dei dati al punto operativo.
156400010601     C     opeini        BEGSR
156500010601     C*
156600010601     C* Inizializzo flag e contatori operativi
156700010601     C                   movel     '0'           flgGiro           1
156800010601     C                   movel     '0'           flgMulti          1
156900010601     C                   movel     '1'           flgStato          1
157000010615     C                   movel     '0'           flgOk             1
157100010601     C                   z-add     *zeros        cntNonEl         10 0
157200010601     C                   z-add     *zeros        depfil            3 0
157300010601     C                   z-add     *zeros        invfil            3 0
157400010601     C*
157500010601     C                   ENDSR
157600010601     C***
157700010601
157800010601
157900050421
158000050421
158100070411
158200070411     C     *pssr         BEGSR
158300070411     C*
158400070411     C                   if        %open(tivin00r)
158500070411     C                   close     tivin00r
158600070411     C                   endif
158700070411     C                   if        %open(fivabwwr)
158800070411     C                   close     fivabwwr
158900070411     C                   endif
159000070411     C                   if        %open(fivatwwr)
159100070411     C                   close     fivatwwr
159200070411     C                   endif
159300070411     C*
159400070411     C* Effettuo la chiamata al CLLE preposto
159500070411     C                   call(e)   'TITVVTC'
159600070411     C                   parm                    parccm
159700070411     C                   parm                    parmbr
159800070411     C                   parm      '2'           paropz
159900070411     C*
160000070411     C                   eval      wrkesito = '2'
160100070411     C*
160200070411     C                   seton                                        LR
160300070411     C*
160400070411     C                   ENDSR     '*CANCL'
160500070411     C***
160600070411
160700050421
160800010601
160900010330
161000010330
161100000613     C     *inzsr        BEGSR
161200990910     C*
161300990910     C     *entry        plist
161400990920     C                   parm                    tivlrds
161500990921     C                   parm      wrkesito      esito
161600000724     C                   parm                    prmlit
161700000710     C                   parm                    prmfir
161800010330     C*
161900010330     C* CALCOLA LA DATA CORRENTE
162000010330     C                   time                    wn14             14 0
162100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
162200010330     C                   z-add     wn8           g08dat
162300010330     C                   z-add     *zeros        g08inv
162400010330     C                   movel     '0'           g08err
162500010330     C                   call      'XSRDA8'
162600010330     C                   parm                    wlbda8
162700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
162800000613     C*
162900000613     C                   ENDSR
163000000613     C***
163100990908
163200061211     Otitvv8p   E            riepilogo         2
163300990915     O                                              'Upload via Internet'
163400990915     O                                           +1 'Traduzione TIVIN00R -'
163500021025     O                                           +1 'FIVABWWR'
163600990915     O                                           +1 'Testate bolle'
163700990915     O          E            riepilogo   2
163800990915     O                       wrkdata
163900990915     O                       wrkora              +1
164000990915     O                       procname            +1
164100990915     O          E            riepilogo   2
164200990915     O                                              'Cliente..................:'
164300010605     O                       VABCCM        z     +1
164400990915     O          E            riepilogo   2
164500990920     O                                              'Riferimento Strategi.....:'
164600990920     O                       vlrhdl              +1
164700990915     O          E            riepilogo   2
164800990915     O                                              'Giusti...................:'
164900040119     O                       §CTROKVB      2   +  1
165000990915     O          E            riepilogo   2
165100990915     O                                              'Sbagliati e corretti.....:'
165200971022     O                       §CTRMO        2   +  1
165300990915     O          E            riepilogo   2
165400990915     O                                              'Sbagliati e scartati.....:'
165500971022     O                       §CTRNO        2   +  1
165600000613
165700061211     Otitvv8ps  E            testdett          2
165800000613     O                                              'Upload via Internet'
165900000613     O                                           +1 'Traduzione TIVIN00R -'
166000021025     O                                           +1 'FIVABWWR'
166100000613     O                                           +1 'Testate bolle'
166200000616     O          E            testdett    3
166300000613     O                                           +2 'N° rec'
166400000613     O                                           +3 'Anteprima contenuto'
166500000616     O          E            rigadett    2
166600000613     O                       rrnum               +2
166700000621     O                       recko               +3
166800000616     O          E            findett     2
166900000613     O                       wrkdata
167000000613     O                       wrkora              +1
167100000613     O                       procname            +1
167200000616     O          E            findett     2
167300000613     O                                              'Cliente..................:'
167400010605     O                       VABCCM        z     +1
167500000616     O          E            findett     2
167600000613     O                                              'Riferimento Strategi.....:'
167700000613     O                       vlrhdl              +1
167800000616     O          E            findett     2
167900000613     O                                              'Giusti...................:'
168000040119     O                       §CTROKVB      2   +  1
168100000616     O          E            findett     2
168200000613     O                                              'Sbagliati e corretti.....:'
168300000613     O                       §CTRMO        2   +  1
168400000616     O          E            findett     2
168500000613     O                                              'Sbagliati e scartati.....:'
168600000613     O                       §CTRNO        2   +  1
168700000613** CMD - COMANDI CL
168800061211OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV87P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
168900061211OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVV8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
169000061211DLTOVR FILE(TITVV8P TITVV8PS) LVL(*)
169100010606
169200000613
169300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
169400080123FILLER1;VATNOT;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNT2;VABRD2;
169500080131VABRMA;VABNCL;VABPKB;VABCAS;VABIAS;VABNOT;VABTC1GC1
169600040113
