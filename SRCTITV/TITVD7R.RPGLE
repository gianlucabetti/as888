000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800040719     Ftitvd7p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100040719     Ftitvd7ps  O    f  198        PRINTER usropn
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
013100040719     C                   if        not %open(titvd7ps)
013200040719     C                   open      titvd7ps
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
016300040719     C                   if        %open(titvd7ps)
016400000616     C                   except    findett
016500040719     C                   close     titvd7ps
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
018500040719     C                   if        not %open(titvd7p)
018600040719     C   50              open      titvd7p
018700990915     C                   endif
018800990915     C*
018900030715     C   50              except    riepilogo
019000990915     C*
019100040719     C                   if        %open(titvd7p)
019200040719     C   50              close     titvd7p
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
038300040719     C* Imposto il tipo record sul file VAT a 'B' x indicare il telefono destinatario
038400040719     C                   EVAL      VATTRC = 'B'
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
040400040719     C                   eval      parccm = '00011850'
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
044800040719     C                   EVAL      VABCCM = 0011850
044900040719     C                   EVAL      VATCCM = 0011850
045000040719     C                   EVAL      VABLNP = 001
045100040719     C                   EVAL      VATLNP = 001
045200040706     C                   EVAL      VABCTR = 000
045300040119     C                   EVAL      VABCBO = '1'
045400040719     C                   EVAL      VATTRC = 'B'
045500030822     C*
045600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
045700030715     C* e delimitatore testo.
045800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
045900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
046000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
046100030822     C*
046200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
046300030715     C                   IF        %subst(vlrppt:1:1) = 'S'
046400030715     C                   SETON                                        50
046500030715     C                   ELSE
046600030715     C                   SETOFF                                       50
046700030715     C                   ENDIF
046800030822     C*
046900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
047000030822     C                   EVAL      posDaDft = 1
047100030822     C                   EVAL      posADft  = 0
047200030822     C                   EVAL      wGiroDft = 0
047300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
047400030822     C                             posDaDft > 0
047500030822     C*
047600030822     C* Gestisco il 1° giro
047700030822     C                   IF        wGiroDft = 0
047800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
047900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
048000030822     C* Incremento il contatore dei "giri"
048100030822     C                   EVAL      wGiroDft = 1
048200030822     C                   ELSE
048300030822     C                   EVAL      posDaDft = posADft
048400030822     C                   ENDIF
048500030822     C* Eseguo lo scan x trovare la fine del campo corrente
048600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
048700030822     C*
048800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
048900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
049000030822     C                   IF        posDaDft > 0 AND
049100030822     C                             posADft  > 0
049200030822     C* NCL
049300030822     C                   IF        %subst(
049400030822     C                             %subst(vlrppt:posDaDft+1:
049500030822     C                             posADft-posDaDft-1):1:3)
049600030822     C                             = 'NCL'
049700030822     C                   EVAL      PiStr=%trim(%subst(
049800030822     C                             %subst(vlrppt:posDaDft+1:
049900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050000030822     C                   EXSR      CHKNUM
050100030822     C                   IF        PiInt=*on
050200030822     C                   Z-ADD     PiVal         VABNCL
050300030822     C                   ENDIF
050400030822     C                   ENDIF
050500030822     C* CCM
050600030822     C                   IF        %subst(
050700030822     C                             %subst(vlrppt:posDaDft+1:
050800030822     C                             posADft-posDaDft-1):1:3)
050900030822     C                             = 'CCM'
051000030822     C                   EVAL      PiStr=%trim(%subst(
051100030822     C                             %subst(vlrppt:posDaDft+1:
051200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051300030822     C                   EXSR      CHKNUM
051400030822     C                   IF        PiInt=*on
051500030822     C                   Z-ADD     PiVal         VABCCM
051600040119     C                   Z-ADD     PiVal         VATCCM
051700030822     C                   ENDIF
051800030822     C                   ENDIF
051900030822     C* LNP
052000030822     C                   IF        %subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):1:3)
052300030822     C                             = 'LNP'
052400030822     C                   EVAL      PiStr=%trim(%subst(
052500030822     C                             %subst(vlrppt:posDaDft+1:
052600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052700030822     C                   EXSR      CHKNUM
052800030822     C                   IF        PiInt=*on
052900030822     C                   Z-ADD     PiVal         VABLNP
053000040119     C                   Z-ADD     PiVal         VATLNP
053100030822     C                   ENDIF
053200030822     C                   ENDIF
053300030822     C* NRS
053400030822     C                   IF        %subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):1:3)
053700030822     C                             = 'NRS'
053800030822     C                   EVAL      PiStr=%trim(%subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054100030822     C                   EXSR      CHKNUM
054200030822     C                   IF        PiInt=*on
054300030822     C                   Z-ADD     PiVal         VABNRS
054400030822     C                   ENDIF
054500030822     C                   ENDIF
054600030822     C* CTR
054700030822     C                   IF        %subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):1:3)
055000030822     C                             = 'CTR'
055100030822     C                   EVAL      PiStr=%trim(%subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055400030822     C                   EXSR      CHKNUM
055500030822     C                   IF        PiInt=*on
055600030822     C                   Z-ADD     PiVal         VABCTR
055700030822     C                   ENDIF
055800030822     C                   ENDIF
055900030822     C* PKB
056000030822     C                   IF        %subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):1:3)
056300030822     C                             = 'PKB'
056400030822     C                   EVAL      PiStr=%trim(%subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056700030822     C                   EXSR      CHKNUM
056800030822     C                   IF        PiNum=*on
056900030822     C                   Z-ADD     PiVal         VABPKB
057000030822     C                   ENDIF
057100030822     C                   ENDIF
057200030822     C* VLB
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'VLB'
057700030822     C                   EVAL      PiStr=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   EXSR      CHKNUM
058100030822     C                   IF        PiNum=*on
058200030822     C                   Z-ADD     PiVal         VABVLB
058300030822     C                   ENDIF
058400030822     C                   ENDIF
058500030822     C* QFT
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'QFT'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiNum=*on
059500030822     C                   Z-ADD     PiVal         VABQFT
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* CBO
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'CBO'
060300030822     C                   EVAL      VABCBO=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   ENDIF
060700030822     C* TSP
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'TSP'
061200030822     C                   EVAL      VABTSP=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   ENDIF
061600030822     C* VAS
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'VAS'
062100030822     C                   EVAL      VABVAS=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   ENDIF
062500030822     C* VCA
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'VCA'
063000030822     C                   EVAL      VABVCA=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   ENDIF
063400030822     C* GCA
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'GCA'
063900030822     C                   EVAL      VABGCA=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   ENDIF
064300030822     C* CTM
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'CTM'
064800030822     C                   EVAL      VABCTM=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   ENDIF
065200030822     C* FFD
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'FFD'
065700030822     C                   EVAL      VABFFD=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   ENDIF
066100030822     C* VAD
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'VAD'
066600030822     C                   EVAL      VABVAD=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* GMA
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'GMA'
067500030822     C                   EVAL      VABGMA=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900030822     C* GGA
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'GGA'
068400030822     C                   EVAL      VABGGA=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   ENDIF
068800030822     C* GVA
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'GVA'
069300030822     C                   EVAL      VABGVA=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   ENDIF
069700030822     C* TC1
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'TC1'
070200030822     C                   EVAL      VABTC1=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600030822     C* TC2
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'TC2'
071100030822     C                   EVAL      VABTC2=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* ...
071600030822     C                   ENDIF
071700030822     C                   ENDDO
071800020204     C*
071900010330     C                   ENDSR
072000010607     C*----------------------------------------------------*
072100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
072200010607     C*----------------------------------------------------*
072300010607     C     IMPVAB        BEGSR
072400010607     C*
072500010607     C                   SETOFF                                       3132
072600030515     C*
072700010607     C                   MOVEL     datcor        VABAAS
072800040119     C                   MOVEL     datcor        VATAAS
072900010607     C                   MOVE      datcor        VABMGS
073000010607     C*
073100030715     C**********
073200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073300030715     C**********
073400030715     C                   IF        wGiro = *zeros
073500040706     C*
073600040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
073700040706     C                   EVAL      depvinDTA = vinDTA
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
078800040706     C* Ripristino la 1° riga originale
078900040706     C                   EVAL      vinDTA = depvinDTA
079000030715     C                   ELSE
079100030715     C                   EVAL      wGiro = 2
079200031204     C**********
079300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
079400031204     C* d separatore campo
079500031204     C**********
079600031204     C                   EVAL      vindta = %trim(vindta)
079700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
079800031204     C                   EVAL      vindta = %subst(vindta:2)
079900031204     C                   ENDDO
080000031204     C*
080100031204     C                   Z-ADD     *zeros        lunghInput        4 0
080200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
080400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
080500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080600031204     C                   ENDDO
080700030715     C**********
080800030715     C* Effettuo lo split del campo dati d input
080900030715     C**********
081000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081100030715     C                   DOW       posDa <= %len(%trim(vindta))
081200030715     C*
081300030715     C* Gestisco il 1° campo
081400030715     C                   IF        i = 1
081500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081600030715     C                   EVAL      posDa = 1
081700030715     C* Eseguo lo scan x trovare la fine del primo campo
081800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081900030715     C* A questo punto "estraggo" il campo corrente
082000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082100030715     C                                                    (posA-posDa))
082200030715     C* X i campi successivi al 1°
082300030715     C                   ELSE
082400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082500030715     C                   EVAL      posDa = posA + 1
082600031204     C* Verifico che nn vi sia il campo nn valorizzato
082700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
082800031204     C* Se campo nn valorizzato skippo al prossimo
082900031204     C                   EVAL      SkSplitCSV(i) = *blanks
083000031204     C                   EVAL      posA  = posA + 1
083100031204     C                   ELSE
083200030715     C* Eseguo lo scan x trovare la fine del campo corrente
083300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083400030715     C* Gestisco l'ultimo campo
083500030715     C                   IF        posA = *zeros
083600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C                   LEAVE
084100030715     C                   ELSE
084200030715     C* A questo punto "estraggo" il campo corrente
084300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084400030715     C                                                    (posA-posDa))
084500030715     C                   ENDIF
084600031204     C                   ENDIF
084700030715     C                   ENDIF
084800030715     C* Incremento il contatore d campo
084900030715     C                   EVAL      i = i +1
085000030715     C                   ENDDO
085100030715     C*
085200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085300030715     C* delimitazione testo
085400030715     C                   EVAL      i = 1
085500030715     C                   DOW       i < %elem(SkSplitCSV)
085600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
085700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
085800030715     C                   EVAL      i = i + 1
085900030715     C                   ENDDO
086000030715     C*
086100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086200030715     C                   EVAL      i = 1
086300030715     C                   DOW       i < %elem(SkSplitFLD)
086400030715     C*
086500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
086600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
086700030715     C                             SkSplitCSV(i) <> *blanks
086800030721     C*** ATB
086900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
087000030721     C                   EVAL      VABATB = SkSplitCSV(i)
087100030721     C                   ENDIF
087200030715     C*** RSD
087300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
087400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
087500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
087600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
087700030715     C* ==
087800030715     C                   ENDIF
087900030721     C*** RD2
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
088100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
088200030721     C                   ENDIF
088300030715     C*** IND
088400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
088500030715     C                   EVAL      VABIND = SkSplitCSV(i)
088600030715     C                   ENDIF
088700030715     C*** LOD
088800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
088900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
089000030715     C                   ENDIF
089100030715     C*** PRD
089200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
089300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
089400030715     C                   ENDIF
089500030721     C*** NZD
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
089700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030715     C*** RMA
090000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
090100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
090200030715     C                   ENDIF
090300030715     C*** NOT
090400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
090500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
090600030715     C                   ENDIF
090700030715     C*** NT2
090800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
090900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
091000030715     C                   ENDIF
091100030715     C*** VCA
091200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
091300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
091400030715     C                   ENDIF
091500030715     C*** VAS
091600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
091700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
091800030715     C                   ENDIF
091900030715     C*** FFD
092000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
092100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
092200030715     C                   ENDIF
092300030721     C*** GC1
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
092500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030721     C*** GC2
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
092900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** TSP
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
093300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** NAS
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
093700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*** TIC
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
094100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030721     C*** GCA
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
094500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*** XCO
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
094900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*** CTM
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
095300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030721     C*** TCR
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
095700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030721     C*** CTS
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
096100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** FTM
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
096500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*** VAD
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
096900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030721     C*** GMA
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
097300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
097400030721     C                   ENDIF
097500030721     C*** GGA
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
097700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030721     C*** GVA
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
098100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030721     C*** TC1
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
098500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030721     C*** TC2
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
098900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030721     C*** SCL
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
099300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030721     C*** RMO
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
099700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** NMO
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
100100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
100200030721     C                   ENDIF
100300040719     C*** CBO
100400040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100500040719     C                   IF        %trim(SkSplitCSV(i)) = 'STANDARD'
100600040719     C                   EVAL      VABCBO = '1'
100700040119     C                   ENDIF
100800040719     C                   IF        %trim(SkSplitCSV(i)) = 'CONTRASSEGNO'
100900040719     C                   EVAL      VABCBO = '4'
101000040719     C                   ENDIF
101100040719     C                   ENDIF
101200030721     C*
101300040706     C* Reperisco quindi i campi numerici...
101400040706     C*** NSP
101500040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
101600040706     C                   EVAL      PiStr=SkSplitCSV(i)
101700040706     C                   EXSR      CHKNUM
101800040706     C                   IF        PiInt=*on
101900040706     C                   Z-ADD     PiVal         VABNSP
102000040706     C                   ELSE
102100040706     C                   SETON                                        31
102200040706     C                   EVAL      VABNSP = *zeros
102300040706     C                   EVAL      vinmsg = %trimr(vinmsg)
102400040706     C                             + ' ' + 'VABNSP'
102500040706     C                   ENDIF
102600040706     C                   ENDIF
102700040706     C*** RMN
102800040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
102900040706     C                   EVAL      PiStr=SkSplitCSV(i)
103000040706     C                   EXSR      CHKNUM
103100040706     C                   IF        PiInt=*on
103200040706     C                   Z-ADD     PiVal         VABRMN
103300040706     C                   ELSE
103400040706     C                   SETON                                        32
103500040706     C                   EVAL      VABRMN = 1
103600040706     C                   EVAL      vinmsg = %trimr(vinmsg)
103700040706     C                             + ' ' + 'VABRMN'
103800040706     C                   ENDIF
103900040706     C                   ENDIF
104000030721     C*** CAD
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
104200030721     C                   EVAL      PiStr=SkSplitCSV(i)
104300030721     C                   EXSR      CHKNUM
104400030721     C                   IF        PiInt=*on
104500030721     C                   Z-ADD     PiVal         Num5_0
104600030721     C                   MOVEL(p)  Num5_0        VABCAD
104700030721     C                   ELSE
104800030721     C                   SETON                                        32
104900030721     C                   EVAL      VABCAD = *zeros
105000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105100030721     C                             + ' ' + 'VABCAD'
105200030721     C                   ENDIF
105300030721     C                   ENDIF
105400030721     C*** NCL
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
105600030721     C                   EVAL      PiStr=SkSplitCSV(i)
105700030721     C                   EXSR      CHKNUM
105800030721     C                   IF        PiInt=*on
105900030721     C                   Z-ADD     PiVal         VABNCL
106000030721     C                   ELSE
106100030721     C                   SETON                                        32
106200030721     C                   EVAL      VABNCL = *zeros
106300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106400040706     C                             + ' ' + 'VABNCL'
106500030721     C                   ENDIF
106600030721     C                   ENDIF
106700030721     C*** PKB
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
106900030721     C                   EVAL      PiStr=SkSplitCSV(i)
107000030721     C                   EXSR      CHKNUM
107100030721     C                   IF        PiNum=*on
107200030721     C                   Z-ADD     PiVal         VABPKB
107300030721     C                   ELSE
107400030721     C                   SETON                                        32
107500030721     C                   EVAL      VABPKB = *zeros
107600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107700030721     C                             + ' ' + 'VABPKB'
107800030721     C                   ENDIF
107900030721     C                   ENDIF
108000030721     C*** CAS
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
108200030721     C                   EVAL      PiStr=SkSplitCSV(i)
108300030721     C                   EXSR      CHKNUM
108400030721     C                   IF        PiNum=*on
108500030721     C                   Z-ADD     PiVal         VABCAS
108600030721     C                   ELSE
108700030721     C                   SETON                                        32
108800030721     C                   EVAL      VABCAS = *zeros
108900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109000030721     C                             + ' ' + 'VABCAS'
109100030721     C                   ENDIF
109200030721     C                   ENDIF
109300030721     C*** CCM
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
109500030721     C                   EVAL      PiStr=SkSplitCSV(i)
109600030721     C                   EXSR      CHKNUM
109700030721     C                   IF        PiInt=*on      AND
109800030721     C                             PiVal<=9999999 AND
109900030721     C                             PiVal>*zeros
110000030721     C                   Z-ADD     PiVal         VABCCM
110100040119     C                   Z-ADD     PiVal         VATCCM
110200030721     C                   ELSE
110300030721     C                   SETON                                        32
110400030721     C                   EVAL      VABCCM = *zeros
110500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110600040119     C                             + ' ' + 'VABCCM VATCCM'
110700030721     C                   ENDIF
110800030721     C                   ENDIF
110900030721     C*** LNP
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
111100030721     C                   EVAL      PiStr=SkSplitCSV(i)
111200030721     C                   EXSR      CHKNUM
111300030721     C                   IF        PiInt=*on    AND
111400030721     C                             PiVal<=999   AND
111500030721     C                             PiVal>*zeros
111600030721     C                   Z-ADD     PiVal         VABLNP
111700040119     C                   Z-ADD     PiVal         VATLNP
111800030721     C                   ELSE
111900030721     C                   SETON                                        32
112000030721     C                   EVAL      VABLNP = *zeros
112100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112200040119     C                             + ' ' + 'VABLNP VATLNP'
112300030721     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** NRS
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
112700030721     C                   EVAL      PiStr=SkSplitCSV(i)
112800030721     C                   EXSR      CHKNUM
112900030721     C                   IF        PiInt=*on AND
113000030721     C                             PiVal<=99
113100030721     C                   Z-ADD     PiVal         VABNRS
113200030721     C                   ELSE
113300030721     C                   SETON                                        32
113400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113500030721     C                             + ' ' + 'VABNRS'
113600030721     C                   ENDIF
113700030721     C                   ENDIF
113800030721     C*** LNA
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
114000030721     C                   EVAL      PiStr=SkSplitCSV(i)
114100030721     C                   EXSR      CHKNUM
114200030721     C                   IF        PiInt=*on    AND
114300030721     C                             PiVal<=999   AND
114400030721     C                             PiVal>*zeros
114500030721     C                   Z-ADD     PiVal         VABLNA
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      VABLNA = *zeros
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000030721     C                             + ' ' + 'VABLNA'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** CTR
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiInt=*on  AND
115800030721     C                             PiVal<=999
115900030721     C                   Z-ADD     PiVal         VABCTR
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABCTR'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** IAS
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiNum=*on
117100030721     C                   Z-ADD     PiVal         VABIAS
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117500030721     C                             + ' ' + 'VABIAS'
117600030721     C                   ENDIF
117700030721     C                   ENDIF
117800030721     C*** VLB
117900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
118000030721     C                   EVAL      PiStr=SkSplitCSV(i)
118100030721     C                   EXSR      CHKNUM
118200030721     C                   IF        PiNum=*on
118300030721     C                   Z-ADD     PiVal         VABVLB
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABVLB'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** QFT
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiNum=*on
119500030721     C                   Z-ADD     PiVal         VABQFT
119600030721     C                   ELSE
119700030721     C                   SETON                                        32
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900030721     C                             + ' ' + 'VABQFT'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** NCD
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on      AND
120700030721     C                             PiVal<=9999999
120800030721     C                   Z-ADD     PiVal         VABNCD
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABNCD'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** NCA
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on      AND
122000030721     C                             PiVal<=9999999
122100030721     C                   Z-ADD     PiVal         VABNCA
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500030721     C                             + ' ' + 'VABNCA'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800030721     C*** ZNC
122900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
123000030721     C                   EVAL      PiStr=SkSplitCSV(i)
123100030721     C                   EXSR      CHKNUM
123200030721     C                   IF        PiInt=*on AND
123300030721     C                             PiVal<=99
123400030721     C                   Z-ADD     PiVal         VABZNC
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABZNC'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** DCR
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on       AND
124600030721     C                             PiVal<=99999999
124700030721     C                   Z-ADD     PiVal         VABDCR
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABDCR'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** HCR
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiInt=*on   AND
125900030721     C                             PiVal<=9999
126000030721     C                   Z-ADD     PiVal         VABHCR
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABHCR'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** VMD
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiNum=*on
127200030721     C                   Z-ADD     PiVal         VABVMD
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABVMD'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** ANT
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on        AND
128400030721     C                             PiVal<=999999999
128500030721     C                   Z-ADD     PiVal         VABANT
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABANT'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** CMO
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on
129700030721     C                   Z-ADD     PiVal         Num5_0
129800030721     C                   MOVEL(p)  Num5_0        VABCMO
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      VABCMO = *zeros
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABCMO'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600040706     C*
130700040719     C*** VATNOT
130800040719     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
130900040719     C                   EVAL      VATNOT = SkSplitCSV(i)
131000040719     C                   ENDIF
131100030715     C*
131200030715     C                   ENDIF
131300030715     C                   EVAL      i = i + 1
131400030715     C                   ENDDO
131500040706     C*
131600040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
131700040706     C* Reperisco la provincia dal CAP e dalla località
131800040706     C                   IF        VABCAD <> *blanks
131900040706     C                   CLEAR                   TISI95DS
132000040706     C                   EVAL      I95TCN = '3'
132100040706     C                   Z-ADD     datcor        I95DAT
132200040706     C                   EVAL      I95CAP = VABCAD
132300040706     C                   EVAL      I95LOC = VABLOD
132400040706     C                   CALL      'TISI95R'
132500040706     C                   PARM                    TISI95DS
132600040706     C                   EVAL      VABPRD = O95PRV
132700040706     C                   ENDIF
132800040719     C*
132900040719     C* Se NSP nn valorizzato considero errore bloccante in traduzione
133000040719     C                   IF        VABNSP = *zeros
133100040719     C                   SETON                                        31
133200040719     C                   ENDIF
133300040719     C*
133400040719     C* Se LNP nn valorizzato considero errore bloccante in traduzione
133500040719     C                   IF        VABLNP = *zeros
133600040719     C                   SETON                                        31
133700040719     C                   ENDIF
133800040706     C*
133900040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
134000040706     C* d valorizzarli sempre entrambi
134100040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
134200040706     C                   Z-ADD     VABNSP        VABRMN
134300040706     C                   ENDIF
134400040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
134500040706     C                   MOVEL     VABRMN        VABRMA
134600040706     C                   ENDIF
134700040113     C*
134800040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
134900040113     C                   IF        VABCAS > *zeros
135000040119     C                   EVAL      VABVCA = 'EUR'
135100040119     C                   IF        VABCBO = '1'
135200040113     C                   EVAL      VABCBO = '4'
135300040119     C                   ENDIF
135400040119     C                   IF        VABCBO = '2'
135500040119     C                   EVAL      VABCBO = '6'
135600040119     C                   ENDIF
135700040113     C                   ENDIF
135800020204     C*
135900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
136000020204     C                   EXSR      CHKIMPDIV
136100030715     C                   ENDIF
136200020204     C*
136300010607     C                   ENDSR
136400010607     C*----------------------------------------------------*
136500010601
136600020204
136700020204     C*----------------------------------------------------*
136800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
136900020204     C*----------------------------------------------------*
137000020204     C     CHKIMPDIV     BEGSR
137100020204     C*
137200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
137300020204     C                   Z-ADD     *zeros        wrkDec            9 9
137400020204     C*
137500020204     C* Come prima cosa effettuo considerazioni sulla divisa
137600020204     C                   IF        vabIAS > *zeros
137700020204     C                   IF        vabVAS <> 'EUR'
137800020204     C                   EVAL      vabVAS =  'ITL'
137900020204     C                   ENDIF
138000020204     C                   ENDIF
138100020204     C*
138200020204     C                   IF        vabCAS > *zeros
138300020204     C                   IF        vabVCA <> 'EUR'
138400020204     C                   EVAL      vabVCA =  'ITL'
138500020204     C                   ENDIF
138600020204     C                   ENDIF
138700020204     C*
138800020204     C                   IF        vabVMD > *zeros
138900020204     C                   IF        vabVAD <> 'EUR'
139000020204     C                   EVAL      vabVAD =  'ITL'
139100020204     C                   ENDIF
139200020204     C                   ENDIF
139300020204     C*
139400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
139500020204     C                   Z-ADD     vabIAS        wrkDec
139600020204     C                   IF        wrkDec > *zeros
139700020204     C                   IF        vabVAS = 'ITL'
139800020204     C                   EVAL      vabIAS = *zeros
139900020204     C                   ENDIF
140000020204     C                   ENDIF
140100020204     C*
140200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
140300020204     C                   Z-ADD     vabCAS        wrkDec
140400020204     C                   IF        wrkDec > *zeros
140500020204     C                   IF        vabVCA = 'ITL'
140600020204     C                   EVAL      vabCAS = *zeros
140700020204     C                   ENDIF
140800020204     C                   ENDIF
140900020204     C*
141000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
141100020204     C                   Z-ADD     vabVMD        wrkDec
141200020204     C                   IF        wrkDec > *zeros
141300020204     C                   IF        vabVAD = 'ITL'
141400020204     C                   EVAL      vabVMD = *zeros
141500020204     C                   ENDIF
141600020204     C                   ENDIF
141700020204     C*
141800020204     C                   ENDSR
141900020204     C***
142000020204
142100010330
142200010330
142300010330     C*----------------------------------------------------*
142400010330     C*  CONTROLLO NUMERICITA' CAMPI
142500010330     C*----------------------------------------------------*
142600010330     C     CHKNUM        BEGSR
142700010330     C*
142800010606     C                   IF        PiDecChr = *blanks
142900030715     C                   EVAL      PiDecChr = CharNUM
143000010606     C                   ENDIF
143100010606     C*
143200010606     C                   CALL(e)   'ISNUMERIC'
143300010330     C                   PARM                    PiStr            30
143400010606     C                   PARM                    PiDecChr          1
143500010330     C                   PARM      *ZEROS        PiVal            30 9
143600010330     C                   PARM      '0'           PiInt             1
143700010330     C                   PARM      '0'           PiNum             1
143800010330     C                   IF        %error
143900010606     C                   EVAL      PiNum=*off
144000010330     C                   ENDIF
144100010330     C*
144200010330     C                   ENDSR
144300010330     C***
144400010330
144500010601
144600010601
144700010601
144800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
144900010601     C     repfil        BEGSR
145000010601     C*
145100010601     C                   if        invfil = *zeros and
145200010601     C                             depfil > *zeros and
145300010629     C                             (vinflg = *blanks or
145400010629     C                              vinflg = *zeros)
145500010601     C
145600010601     C                   eval      invfil = depfil
145700010601     C                   endif
145800010601     C*
145900010601     C                   if        depfil <> invfil and
146000010601     C                             invfil > *zeros
146100010601     C                   eval      flgMulti = '1'
146200010601     C                   if        vinflg = *blanks
146300010601     C                   add       1             cntNonEl
146400010601     C                   endif
146500010601     C                   endif
146600010601     C*
146700010601     C                   if        vinflg = '2'
146800010601     C                   eval      flgStato = '2'
146900010601     C                   endif
147000010601     C*
147100010601     C                   ENDSR
147200010601     C***
147300010601
147400010601
147500010601
147600010330
147700040119      /TITLE Invio dei dati al punto operativo.
147800040119     C     invio         BEGSR
147900040119     C*
148000040119     C* 1° invio FIVAT
148100040119     C                   reset                   dscmz
148200040119     C                   move      vlrpoi        cmzdst
148300040119     C                   eval      cmzfld = 'FIVATWWR'
148400040119     C                   eval      cmzmbd = vlrhdl
148500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
148600040119     C***                if        prmfir = *blanks
148700040119     C                   eval      cmzfla = 'FIVAT00F'
148800040119     C                   eval      cmzmba = 'FIVAT00F'
148900040119     C***                else
149000040119     C***                eval      cmzfla = prmfir
149100040119     C***                eval      cmzmba = prmfir
149200040119     C***                endif
149300040119     C                   eval      cmznrr = *zeros
149400040119     C                   move      §ctrokvt      cmznrr
149500040119     C                   eval      cmzlba = vlrfl1
149600040119     C                   call(e)   'TIS711C'
149700040119     C                   parm                    dscmz
149800040119     C                   parm      *blanks       esito
149900040119     C                   if        %error
150000040119     C                             or cmzerr = '1'
150100040119     C                             or esito  = '1'
150200040119     C                   eval      wrkesito = '3'
150300040119     C                   else
150400040119     C*
150500040119     C* 2° invio FIVAB
150600040119     C                   reset                   dscmz
150700040119     C                   move      vlrpoi        cmzdst
150800040119     C                   eval      cmzfld = vlrfou
150900040119     C                   eval      cmzmbd = vlrhdl
151000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
151100040119     C***                if        prmfir = *blanks
151200040119     C                   eval      cmzfla = 'FIVAB00F'
151300040119     C                   eval      cmzmba = 'FIVAB00F'
151400040119     C***                else
151500040119     C***                eval      cmzfla = prmfir
151600040119     C***                eval      cmzmba = prmfir
151700040119     C***                endif
151800040119     C                   eval      cmznrr = *zeros
151900040119     C                   move      §ctrokvb      cmznrr
152000040119     C                   eval      cmzlba = vlrfl1
152100040119     C                   call(e)   'TIS711C'
152200040119     C                   parm                    dscmz
152300040119     C                   parm      *blanks       esito
152400040119     C                   if        %error
152500040119     C                             or cmzerr = '1'
152600040119     C                             or esito  = '1'
152700040119     C                   eval      wrkesito = '3'
152800040119     C                   endif
152900040119     C                   endif
153000040119     C*
153100040119     C                   ENDSR
153200040119     C***
153300010601
153400010601
153500010601
153600010601
153700010601      /TITLE Invio dei dati al punto operativo.
153800010601     C     opeini        BEGSR
153900010601     C*
154000010601     C* Inizializzo flag e contatori operativi
154100010601     C                   movel     '0'           flgGiro           1
154200010601     C                   movel     '0'           flgMulti          1
154300010601     C                   movel     '1'           flgStato          1
154400010615     C                   movel     '0'           flgOk             1
154500010601     C                   z-add     *zeros        cntNonEl         10 0
154600010601     C                   z-add     *zeros        depfil            3 0
154700010601     C                   z-add     *zeros        invfil            3 0
154800010601     C*
154900010601     C                   ENDSR
155000010601     C***
155100010601
155200010601
155300050421
155400050421
155500070411
155600070411     C     *pssr         BEGSR
155700070411     C*
155800070411     C                   if        %open(tivin00r)
155900070411     C                   close     tivin00r
156000070411     C                   endif
156100070411     C                   if        %open(fivabwwr)
156200070411     C                   close     fivabwwr
156300070411     C                   endif
156400070411     C                   if        %open(fivatwwr)
156500070411     C                   close     fivatwwr
156600070411     C                   endif
156700070411     C*
156800070411     C* Effettuo la chiamata al CLLE preposto
156900070411     C                   call(e)   'TITVVTC'
157000070411     C                   parm                    parccm
157100070411     C                   parm                    parmbr
157200070411     C                   parm      '2'           paropz
157300070411     C*
157400070411     C                   eval      wrkesito = '2'
157500070411     C*
157600070411     C                   seton                                        LR
157700070411     C*
157800070411     C                   ENDSR     '*CANCL'
157900070411     C***
158000050421
158100010601
158200010330
158300010330
158400000613     C     *inzsr        BEGSR
158500990910     C*
158600990910     C     *entry        plist
158700990920     C                   parm                    tivlrds
158800990921     C                   parm      wrkesito      esito
158900000724     C                   parm                    prmlit
159000000710     C                   parm                    prmfir
159100010330     C*
159200010330     C* CALCOLA LA DATA CORRENTE
159300010330     C                   time                    wn14             14 0
159400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
159500010330     C                   z-add     wn8           g08dat
159600010330     C                   z-add     *zeros        g08inv
159700010330     C                   movel     '0'           g08err
159800010330     C                   call      'XSRDA8'
159900010330     C                   parm                    wlbda8
160000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
160100000613     C*
160200000613     C                   ENDSR
160300000613     C***
160400990908
160500040719     Otitvd7p   E            riepilogo         2
160600990915     O                                              'Upload via Internet'
160700990915     O                                           +1 'Traduzione TIVIN00R -'
160800021025     O                                           +1 'FIVABWWR'
160900990915     O                                           +1 'Testate bolle'
161000990915     O          E            riepilogo   2
161100990915     O                       wrkdata
161200990915     O                       wrkora              +1
161300990915     O                       procname            +1
161400990915     O          E            riepilogo   2
161500990915     O                                              'Cliente..................:'
161600010605     O                       VABCCM        z     +1
161700990915     O          E            riepilogo   2
161800990920     O                                              'Riferimento Strategi.....:'
161900990920     O                       vlrhdl              +1
162000990915     O          E            riepilogo   2
162100990915     O                                              'Giusti...................:'
162200040119     O                       §CTROKVB      2   +  1
162300990915     O          E            riepilogo   2
162400990915     O                                              'Sbagliati e corretti.....:'
162500971022     O                       §CTRMO        2   +  1
162600990915     O          E            riepilogo   2
162700990915     O                                              'Sbagliati e scartati.....:'
162800971022     O                       §CTRNO        2   +  1
162900000613
163000040719     Otitvd7ps  E            testdett          2
163100000613     O                                              'Upload via Internet'
163200000613     O                                           +1 'Traduzione TIVIN00R -'
163300021025     O                                           +1 'FIVABWWR'
163400000613     O                                           +1 'Testate bolle'
163500000616     O          E            testdett    3
163600000613     O                                           +2 'N° rec'
163700000613     O                                           +3 'Anteprima contenuto'
163800000616     O          E            rigadett    2
163900000613     O                       rrnum               +2
164000000621     O                       recko               +3
164100000616     O          E            findett     2
164200000613     O                       wrkdata
164300000613     O                       wrkora              +1
164400000613     O                       procname            +1
164500000616     O          E            findett     2
164600000613     O                                              'Cliente..................:'
164700010605     O                       VABCCM        z     +1
164800000616     O          E            findett     2
164900000613     O                                              'Riferimento Strategi.....:'
165000000613     O                       vlrhdl              +1
165100000616     O          E            findett     2
165200000613     O                                              'Giusti...................:'
165300040119     O                       §CTROKVB      2   +  1
165400000616     O          E            findett     2
165500000613     O                                              'Sbagliati e corretti.....:'
165600000613     O                       §CTRMO        2   +  1
165700000616     O          E            findett     2
165800000613     O                                              'Sbagliati e scartati.....:'
165900000613     O                       §CTRNO        2   +  1
166000000613** CMD - COMANDI CL
166100040719OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVD7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
166200040719OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVD7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
166300040719DLTOVR FILE(TITVD7P TITVD7PS) LVL(*)
166400010606
166500000613
166600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
166700040719VABCBO;FILLER1;VABNSP;FILLER2;VABRMA;VABRSD;VABIND;VABLOD;VABPRD;
166800040719VABCAD;VATNOT;VABCAS;VABNCL;VABPKB
166900040113
