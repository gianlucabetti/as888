000100050722      /TITLE Upload via Internet: traduzione in FIVAB/FIVAT. (Cliente DGM S.p.A. - Cod. 1090734)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050722     FFIVATwwr  O    E             DISK    usropn
000700021025     FFIVABwwr  O    E             DISK    usropn
000800030715     Ftitvxlp   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030715     Ftitvxlps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400000313     D*
001500000613     D*------------
001600000613     D* COMANDI
001700000613     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900040726     D*------------
002000040726     D* SCHIEDE A PROGRAMMA
002100040726     D*------------
002200050321     D intcol          S            100    DIM(10) CTDATA PERRCD(1)
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
003900050722     D parccm          s              8    INZ(*blanks)
004000050722     D parmbr          s             10    INZ(*blanks)
004100050722     D paropz          s              1    INZ(*blanks)
004200050722     D chkcall         s              1    INZ(*blanks)
004300040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004400030715     D*------------------
004500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004600030715     D*------------------
004700030715     D SkSplitFLD      S             10    DIM(100)
004800030715     D SkSplitCSV      S            256    DIM(100)
004900030715     D CharCSV         S              1
005000030715     D CharTXT         S              1
005100030715     D CharNUM         S              1
005200030715     D posDa           S              3  0 INZ(*zeros)
005300030715     D posA            S              3  0 INZ(*zeros)
005400030715     D i               s              3  0 INZ(1)
005500030715     D wGiro           s              1  0 INZ(*zeros)
005600030822     D*------------------
005700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005800030822     D*------------------
005900030822     D posDaDft        S              3  0 INZ(*zeros)
006000030822     D posADft         S              3  0 INZ(*zeros)
006100030822     D j               s              3  0 INZ(1)
006200030822     D wGiroDft        s              1  0 INZ(*zeros)
006300010330     D*------------------
006400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006500010330     D*------------------
006600010330     D WLBDA8          DS                  INZ
006700010330     D  G08DAT                 1      8  0
006800010330     D  G08INV                 9     16  0
006900010330     D  G08ERR                17     17
007000010330     D  G08TGI                18     22  0
007100031201     D*------------------
007200031201     D* Costanti
007300031201     D*------------------
007400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007600010330
007700010330
007800010330
007900990915     C                   time                    wrkdata
008000990915     C                   time                    wrkora
008100000913     C                   reset                   rrnum
008200990921     C                   reset                   esito
008300990921     C                   reset                   wrkesito
008400010601     C*
008500010601     C                   exsr      opeini
008600010605     C                   exsr      rwvab
008700010601     C*
008800010601     C                   seton                                        lr
008900010601
009000010601
009100010601
009200010601
009300010601     C*--------------------------------------------------------
009400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009500010601     C*--------------------------------------------------------
009600010601     C     PREELA        BEGSR
009700010601     C*
009800010601     C* SE OCCORRE SPEDIRE IN FILIALE
009900010601     C                   if        invfil <> *zeros and
010000010601     C                             flgGiro = '0'
010100010601     C*
010200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010300010601     C                   eval      flgGiro = '1'
010400000724     C*
010500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010700010601     C     invfil        chain     azorg01l
010800000724     C                   if        %found
010900000724     C                   movel(p)  CMD(1)        depcmd
011000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011100000724     C*
011200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011300011113     C                   Z-ADD     150           LENGH            15 5
011400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011500000724     C                   PARM                    depcmd
011600000724     C                   PARM                    LENGH
011700000724     C*
011800000724     C                   endif
011900000616     C*
012000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012100000616     C                   movel(p)  CMD(2)        depcmd
012200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012300000616     C*
012400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012500011113     C                   Z-ADD     150           LENGH            15 5
012600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012700000616     C                   PARM                    depcmd
012800000616     C                   PARM                    LENGH
012900000613     C*
013000030715     C                   if        not %open(titvxlps)
013100030715     C                   open      titvxlps
013200030715     C                   except    testdett
013300000613     C                   endif
013400010601     C*
013500010601     C                   endif
013600010601     C*
013700010601     C                   ENDSR
013800010601     C***
013900010601
014000010601
014100010601
014200010601
014300010601
014400010601
014500010601
014600010601     C*--------------------------------------------------------
014700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014800010601     C*--------------------------------------------------------
014900010601     C     ENDELA        BEGSR
015000990915     C*
015100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015200050722     C*
015300050722     C* Effettuo la chiamata al CLLE preposto
015400050722     C                   call(e)   'TITVVTC'
015500050722     C                   parm                    parccm
015600050722     C                   parm                    parmbr
015700050722     C                   parm      '2'           paropz
015800000613     C*
015900030715     C                   if        %open(titvxlps)
016000000616     C                   except    findett
016100030715     C                   close     titvxlps
016200000613     C                   endif
016300000616     C*
016400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016500011113     C                   Z-ADD     150           LENGH            15 5
016600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
016700000616     C                   PARM                    CMD(3)
016800000616     C                   PARM                    LENGH
016900000616     C*
017000010601     C                   ENDSR
017100010601     C***
017200010601
017300010601
017400010601
017500010601
017600000613
017700000613     C*--------------------------------------------------------
017800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017900000613     C*--------------------------------------------------------
018000000621     C     STPR          BEGSR
018100000613     C*
018200030715     C                   if        not %open(titvxlp)
018300030715     C   50              open      titvxlp
018400990915     C                   endif
018500990915     C*
018600030715     C   50              except    riepilogo
018700990915     C*
018800030715     C                   if        %open(titvxlp)
018900030715     C   50              close     titvxlp
019000990914     C                   endif
019100000613     C*
019200000613     C                   ENDSR
019300000613     C***
019400000613
019500010330
019600010601
019700010601
019800010601
019900010330     C*--------------------------------------------------------
020000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020100010330     C*--------------------------------------------------------
020200010605     C     RWVAB         BEGSR
020300010330     C*
020400010330     C                   if        not %open(tivin00r)
020500010330     C                   open      tivin00r
020600010330     C                   endif
020700021025     C                   if        not %open(fivabwwr)
020800021025     C                   open      fivabwwr
020900010330     C                   endif
021000050722     C*
021100050722     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021200050722     C                   exsr      prevat
021300050722     C*
021400050722     C                   if        chkcall = '0'
021500050722     C*
021600050722     C                   if        not %open(fivatwwr)
021700050722     C                   open      fivatwwr
021800050722     C                   endif
021900010330     C*
022000050722     C                   clear                   §CTROKVB          7 0
022100050722     C                   clear                   §CTROKVT          7 0
022200050722     C                   clear                   §CTRMO            7 0
022300050722     C                   clear                   §CTRNO            7 0
022400010330     C*
022500030822     C                   do        *HIVAL
022600010330     C*
022700030822     C                   read      tivin00r                               70
022800010618     C*
022900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023000010618     C                   if        vinflg = '1'
023100010618     C                   eval      flgOk = '1'
023200010618     C                   endif
023300010618     C*
023400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
023500040510     C                   z-add     *zeros        wLenVINDTA        4 0
023600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
023700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
023800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
023900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024000040510     C                   z-add     1             wFlgVINDTA
024100040510     C                   leave
024200040510     C                   endif
024300040510     C                   enddo
024400040510     C*
024500040218     C                   if        vindta > *blanks AND
024600040510     C                             wFlgVINDTA = 1
024700010330     C                   add       1             rrnum
024800010330     C*
024900010601     C                   if        *in70 = *off and
025000010330     C                             (vinflg = *blanks
025100010330     C                              or vinflg = '0'
025200010330     C                              or vinflg = '2')
025300010330     C*
025400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025500010711     C                   if        vinflg = *blanks or vinflg = '0'
025600010711     C                   clear                   vinmsg
025700010711     C                   endif
025800010601     C*
025900010330     C                   exsr      inzvar
026000010605     C                   exsr      impvab
026100010601     C*
026200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026300010605     C                   eval      depfil = VABLNP
026400010601     C                   exsr      repfil
026500010601     C                   if        depfil = invfil
026600021025     C                   if        vlrpoi = 999
026700030822     C                   move(P)   invfil        VABFGS
026800050722     C                   move(P)   invfil        VATFGS
026900021025     C                   else
027000030822     C                   move(P)   vlrpoi        VABFGS
027100050722     C                   move(P)   vlrpoi        VATFGS
027200021025     C                   endif
027300010601     C*
027400010601     C                   exsr      PREELA
027500010601     C*
027600010604     C* Ebbene...
027700010604     C*
027800050722     C  N31              add       1             §CTROKVB
027900050722     C   32              add       1             §CTRMO
028000050722     C   31              add       1             §CTRNO
028100030822     C                   if        wGiro = 2
028200030822     C  N31              write     FIVAB000
028300050722     C  N31              exsr      wrivat
028400030822     C                   endif
028500010604     C*
028600010604     C                   if        *in31 = *off and
028700010604     C                             *in32 = *off
028800010604     C                   eval      vinflg = '1'
028900010604     C                   else
029000010604     C                   eval      recko = vindta
029100010604     C                   except    rigadett
029200010604     C                   eval      vinflg = '2'
029300010604     C                   endif
029400010604     C                   endif
029500010601     C*
029600010604     C                   endif
029700010604     C*
029800010330     C                   else
029900010330     C                   eval      vinflg = '1'
030000010330     C                   endif
030100010601     C*
030200010601     C  N70              update    tivin000
030300010330     C*
030400030822     C  N70              enddo
030500050722     C*
030600050722     C                   endif                                                  (endif - chkcall)
030700010601     C*
030800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030900010601     C                   if        cntNonEl = *zeros or
031000010601     C                             flgMulti = '0'
031100010330     C* Se non ci sono record con errori ...
031200010601     C                   if        §ctrno = 0 and
031300010604     C                             §ctrmo = 0 and
031400010601     C                             flgStato <> '2'
031500010330     C* ... restituisco esito OK.
031600010330     C                   eval      wrkesito = '0'
031700010330     C                   else
031800050722     C                   if        §ctrokvb > 0
031900010330     C                   eval      wrkesito = '1'
032000010330     C                   else
032100010615     C                   if        flgOk = '0'
032200010615     C                   eval      wrkesito = '2'
032300010615     C                   else
032400010615     C                   eval      wrkesito = '6'
032500010615     C                   endif
032600010330     C                   endif
032700010330     C                   endif
032800010601     C                   else
032900010601     C                   eval      wrkesito = '9'
033000010601     C                   endif
033100010330     C*
033200010330     C                   if        %open(tivin00r)
033300010330     C                   close     tivin00r
033400010330     C                   endif
033500021025     C                   if        %open(fivabwwr)
033600021025     C                   close     fivabwwr
033700010330     C                   endif
033800050722     C                   if        %open(fivatwwr)
033900050722     C                   close     fivatwwr
034000050722     C                   endif
034100010601     C*
034200010601     C                   if        vlrpoi <> 999
034300010601     C                   eval      invfil = vlrpoi
034400010601     C                   endif
034500010330     C*
034600050722     C                   if        §ctrokvb > 0
034700010601     C                             and invfil > *zeros
034800010330     C                   exsr      invio
034900010330     C                   endif
035000010601     C*
035100010618     C                   if        flgGiro = '1'
035200010601     C                   exsr      endela
035300010618     C                   endif
035400010330     C*
035500010330     C                   ENDSR
035600010330     C***
035700050722
035800050722
035900050722
036000050722     C*----------------------------------------------------*
036100050722     C*  SCARICAMENTO BUFFER RECORDS VAT
036200050722     C*----------------------------------------------------*
036300050722     C     WRIVAT        BEGSR
036400050722     C*
036500050722     C* Attribuisco la stessa chiave della testata bolle
036600050722     C                   EVAL      VATAAS = VABAAS
036700050722     C                   EVAL      VATLNP = VABLNP
036800050722     C                   EVAL      VATNRS = VABNRS
036900050722     C                   EVAL      VATNSP = VABNSP
037000050722     C                   EVAL      VATCCM = VABCCM
037100050722     C*
037200050722     C* Imposto il tipo record sul file VAT a 'B' x indicare il telefono destinatario
037300050722     C                   EVAL      VATTRC = 'B'
037400050722     C*
037500050722     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
037600050722     C                   IF        VATNOT <> *blanks
037700050722     C                   WRITE     FIVAT000
037800050722     C                   ADD       1             §CTROKVT
037900050722     C                   ENDIF
038000050722     C*
038100050722     C                   ENDSR
038200050722
038300050722
038400050722
038500050722     C*----------------------------------------------------*
038600050722     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
038700050722     C*----------------------------------------------------*
038800050722     C     PREVAT        BEGSR
038900050722     C*
039000050722     C* Compongo il nome del membro da dare al FIVATWWR
039100050722     C                   eval      parmbr = vlrhdl
039200050722     C                   movel     'M'           parmbr
039300050722     C                   eval      parccm = vlrksc
039400050722     C                   eval      paropz = '1'
039500050722     C* Effettuo la chiamata al CLLE preposto
039600050722     C                   call(e)   'TITVVTC'
039700050722     C                   parm                    parccm
039800050722     C                   parm                    parmbr
039900050722     C                   parm                    paropz
040000050722     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040100050722     C                   if        %error
040200050722     C                   movel     '1'           chkcall
040300050722     C                   else
040400050722     C                   movel     '0'           chkcall
040500050722     C                   endif
040600050722     C*
040700050722     C                   ENDSR
040800010601
040900010601
041000010601
041100010330     C*----------------------------------------------------*
041200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
041300010330     C*----------------------------------------------------*
041400010330     C     INZVAR        BEGSR
041500010330     C*
041600030822     C* Inizializzo variabili di wrk
041700020204     C                   Z-ADD     *zeros        Num5_0            5 0
041800030822     C*
041900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
042000030822     C                   CLEAR                   FIVAB000
042100050722     C                   CLEAR                   FIVAT000
042200030822     C                   CLEAR                   SkSplitCSV
042300030822     C                   EVAL      i = 1
042400030822     C                   EVAL      posDa = *zeros
042500030822     C                   EVAL      posA  = *zeros
042600030822     C*
042700030822     C* Reimposto i valori di default
042800030822     C                   EXSR      DEFCAM
042900030822     C*
043000010330     C                   ENDSR
043100010330     C*----------------------------------------------------*
043200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
043300010330     C*----------------------------------------------------*
043400010330     C     DEFCAM        BEGSR
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
049600030822     C                   ENDIF
049700030822     C                   ENDIF
049800030822     C* LNP
049900030822     C                   IF        %subst(
050000030822     C                             %subst(vlrppt:posDaDft+1:
050100030822     C                             posADft-posDaDft-1):1:3)
050200030822     C                             = 'LNP'
050300030822     C                   EVAL      PiStr=%trim(%subst(
050400030822     C                             %subst(vlrppt:posDaDft+1:
050500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050600030822     C                   EXSR      CHKNUM
050700030822     C                   IF        PiInt=*on
050800030822     C                   Z-ADD     PiVal         VABLNP
050900030822     C                   ENDIF
051000030822     C                   ENDIF
051100030822     C* NRS
051200030822     C                   IF        %subst(
051300030822     C                             %subst(vlrppt:posDaDft+1:
051400030822     C                             posADft-posDaDft-1):1:3)
051500030822     C                             = 'NRS'
051600030822     C                   EVAL      PiStr=%trim(%subst(
051700030822     C                             %subst(vlrppt:posDaDft+1:
051800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051900030822     C                   EXSR      CHKNUM
052000030822     C                   IF        PiInt=*on
052100030822     C                   Z-ADD     PiVal         VABNRS
052200030822     C                   ENDIF
052300030822     C                   ENDIF
052400030822     C* CTR
052500030822     C                   IF        %subst(
052600030822     C                             %subst(vlrppt:posDaDft+1:
052700030822     C                             posADft-posDaDft-1):1:3)
052800030822     C                             = 'CTR'
052900030822     C                   EVAL      PiStr=%trim(%subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053200030822     C                   EXSR      CHKNUM
053300030822     C                   IF        PiInt=*on
053400030822     C                   Z-ADD     PiVal         VABCTR
053500030822     C                   ENDIF
053600030822     C                   ENDIF
053700030822     C* PKB
053800030822     C                   IF        %subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):1:3)
054100030822     C                             = 'PKB'
054200030822     C                   EVAL      PiStr=%trim(%subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054500030822     C                   EXSR      CHKNUM
054600030822     C                   IF        PiNum=*on
054700030822     C                   Z-ADD     PiVal         VABPKB
054800030822     C                   ENDIF
054900030822     C                   ENDIF
055000030822     C* VLB
055100030822     C                   IF        %subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):1:3)
055400030822     C                             = 'VLB'
055500030822     C                   EVAL      PiStr=%trim(%subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055800030822     C                   EXSR      CHKNUM
055900030822     C                   IF        PiNum=*on
056000030822     C                   Z-ADD     PiVal         VABVLB
056100030822     C                   ENDIF
056200030822     C                   ENDIF
056300030822     C* QFT
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'QFT'
056800030822     C                   EVAL      PiStr=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   EXSR      CHKNUM
057200030822     C                   IF        PiNum=*on
057300030822     C                   Z-ADD     PiVal         VABQFT
057400030822     C                   ENDIF
057500030822     C                   ENDIF
057600030822     C* CBO
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'CBO'
058100030822     C                   EVAL      VABCBO=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   ENDIF
058500030822     C* TSP
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'TSP'
059000030822     C                   EVAL      VABTSP=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   ENDIF
059400030822     C* VAS
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'VAS'
059900030822     C                   EVAL      VABVAS=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   ENDIF
060300030822     C* VCA
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'VCA'
060800030822     C                   EVAL      VABVCA=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   ENDIF
061200030822     C* GCA
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'GCA'
061700030822     C                   EVAL      VABGCA=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   ENDIF
062100030822     C* CTM
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'CTM'
062600030822     C                   EVAL      VABCTM=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   ENDIF
063000030822     C* FFD
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'FFD'
063500030822     C                   EVAL      VABFFD=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   ENDIF
063900030822     C* VAD
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'VAD'
064400030822     C                   EVAL      VABVAD=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   ENDIF
064800030822     C* GMA
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'GMA'
065300030822     C                   EVAL      VABGMA=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* GGA
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'GGA'
066200030822     C                   EVAL      VABGGA=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* GVA
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'GVA'
067100030822     C                   EVAL      VABGVA=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* TC1
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'TC1'
068000030822     C                   EVAL      VABTC1=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400030822     C* TC2
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'TC2'
068900030822     C                   EVAL      VABTC2=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   ENDIF
069300030822     C* ...
069400030822     C                   ENDIF
069500030822     C                   ENDDO
069600020204     C*
069700010330     C                   ENDSR
069800010607     C*----------------------------------------------------*
069900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
070000010607     C*----------------------------------------------------*
070100010607     C     IMPVAB        BEGSR
070200010607     C*
070300010607     C                   SETOFF                                       3132
070400030515     C*
070500010607     C                   MOVEL     datcor        VABAAS
070600010607     C                   MOVE      datcor        VABMGS
070700010607     C*
070800030715     C**********
070900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
071000030715     C**********
071100030715     C                   IF        wGiro = *zeros
071200040726     C*
071300040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
071400040726     C                   EVAL      depvinDTA = vinDTA
071500040726     C*
071600040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
071700040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
071800050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
071900050321     C                                      %trim(intcol(5))+%trim(intcol(6))+
072000050321     C                                      %trim(intcol(7))+%trim(intcol(8))+
072100050321     C                                      %trim(intcol(9))+%trim(intcol(10))
072200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
072300031201     C     minu:maiu     XLATE     vindta        vindta
072400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
072500040324     C                   DOW       posDa <= %len(%trimr(vindta))
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
074400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
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
076800040726     C* Ripristino la 1° riga originale
076900040726     C                   EVAL      vinDTA = depvinDTA
077000030715     C                   ELSE
077100030715     C                   EVAL      wGiro = 2
077200031204     C**********
077300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
077400031204     C* d separatore campo
077500031204     C**********
077600040726     C                   EVAL      vindta = %trim(vindta)
077700040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
077800040726     C                   EVAL      vindta = %subst(vindta:2)
077900040726     C                   ENDDO
078000031204     C*
078100031204     C                   Z-ADD     *zeros        lunghInput        4 0
078200040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
078300040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
078400040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
078500040726     C                   EVAL      lunghInput = %len(%trim(vindta))
078600040726     C                   ENDDO
078700030715     C**********
078800030715     C* Effettuo lo split del campo dati d input
078900030715     C**********
079000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
079100040324     C                   DOW       posDa <= %len(%trimr(vindta))
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
081600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
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
086300050722     C*** VATNOT (tipo record 'B')
086400050722     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
086500050722     C                   EVAL      VATNOT = SkSplitCSV(i)
086600050722     C                   ENDIF
086700030715     C*** IND
086800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
086900030715     C                   EVAL      VABIND = SkSplitCSV(i)
087000030715     C                   ENDIF
087100030715     C*** LOD
087200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
087300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
087400030715     C                   ENDIF
087500030715     C*** PRD
087600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
087700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
087800030715     C                   ENDIF
087900030721     C*** NZD
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
088100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
088200030721     C                   ENDIF
088300030715     C*** RMA
088400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
088500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
088600030715     C                   ENDIF
088700030715     C*** NOT
088800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
088900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
089000030715     C                   ENDIF
089100030715     C*** NT2
089200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
089300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
089400030715     C                   ENDIF
089500030715     C*** VCA
089600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
089700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
089800030715     C                   ENDIF
089900030715     C*** VAS
090000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
090100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
090200030715     C                   ENDIF
090300030715     C*** FFD
090400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
090500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
090600030715     C                   ENDIF
090700030715     C*** CBO
090800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
090900030715     C                   EVAL      VABCBO = SkSplitCSV(i)
091000030715     C                   ENDIF
091100030715     C
091200030721     C*** GC1
091300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
091400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
091500030721     C                   ENDIF
091600030721     C*** GC2
091700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
091800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
091900030721     C                   ENDIF
092000030721     C*** TSP
092100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
092200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
092300030721     C                   ENDIF
092400030721     C*** NAS
092500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
092600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
092700030721     C                   ENDIF
092800030721     C*** TIC
092900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
093000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
093100030721     C                   ENDIF
093200030721     C*** GCA
093300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
093400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
093500030721     C                   ENDIF
093600030721     C*** XCO
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
093800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030721     C*** CTM
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
094200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030721     C*** TCR
094500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
094600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
094700030721     C                   ENDIF
094800030721     C*** CTS
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
095000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030721     C*** FTM
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
095400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030721     C*** VAD
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
095800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** GMA
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
096200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** GGA
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
096600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** GVA
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
097000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** TC1
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
097400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** TC2
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
097800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** SCL
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
098200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** RMO
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
098600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** NMO
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
099000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*
099300030721     C* Reperisco quindi i campi numerici...
099400030721     C*** CAD
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
099600030721     C                   EVAL      PiStr=SkSplitCSV(i)
099700030721     C                   EXSR      CHKNUM
099800030721     C                   IF        PiInt=*on
099900030721     C                   Z-ADD     PiVal         Num5_0
100000030721     C                   MOVEL(p)  Num5_0        VABCAD
100100030721     C                   ELSE
100200030721     C                   SETON                                        32
100300030721     C                   EVAL      VABCAD = *zeros
100400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100500030721     C                             + ' ' + 'VABCAD'
100600030721     C                   ENDIF
100700030721     C                   ENDIF
100800030721     C*** RMN
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
101000030721     C                   EVAL      PiStr=SkSplitCSV(i)
101100030721     C                   EXSR      CHKNUM
101200030721     C                   IF        PiInt=*on
101300030721     C                   Z-ADD     PiVal         VABRMN
101400030721     C                   ELSE
101500030721     C                   SETON                                        32
101600030721     C                   EVAL      VABRMN = 1
101700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101800030721     C                             + ' ' + 'VABRMN'
101900030721     C                   ENDIF
102000030721     C                   ENDIF
102100030721     C*** NSP
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
102300030721     C                   EVAL      PiStr=SkSplitCSV(i)
102400030721     C                   EXSR      CHKNUM
102500040726     C                   IF        PiInt=*on
102600040726     C                   Z-ADD     PiVal         VABNSP
102700040802     C*                  IF        PiVal>9999999
102800040802     C*                  SETON                                        32
102900040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
103000040802     C*                            + ' ' + 'VABNSP'
103100040802     C*                  ENDIF
103200030721     C                   ELSE
103300030721     C                   SETON                                        32
103400030721     C                   EVAL      VABNSP = *zeros
103500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103600030721     C                             + ' ' + 'VABNSP'
103700030721     C                   ENDIF
103800030721     C                   ENDIF
103900030721     C*** NCL
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
104100030721     C                   EVAL      PiStr=SkSplitCSV(i)
104200030721     C                   EXSR      CHKNUM
104300030721     C                   IF        PiInt=*on
104400030721     C                   Z-ADD     PiVal         VABNCL
104500030721     C                   ELSE
104600030721     C                   SETON                                        32
104700030721     C                   EVAL      VABNCL = *zeros
104800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104900030721     C                             + ' ' + 'VABNCL'
105000030721     C                   ENDIF
105100030721     C                   ENDIF
105200030721     C*** PKB
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
105400030721     C                   EVAL      PiStr=SkSplitCSV(i)
105500030721     C                   EXSR      CHKNUM
105600030721     C                   IF        PiNum=*on
105700030721     C                   Z-ADD     PiVal         VABPKB
105800030721     C                   ELSE
105900030721     C                   SETON                                        32
106000030721     C                   EVAL      VABPKB = *zeros
106100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106200030721     C                             + ' ' + 'VABPKB'
106300030721     C                   ENDIF
106400030721     C                   ENDIF
106500050505     C*** CAS
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
106700030721     C                   EVAL      PiStr=SkSplitCSV(i)
106800030721     C                   EXSR      CHKNUM
106900030721     C                   IF        PiNum=*on
107000030721     C                   Z-ADD     PiVal         VABCAS
107100030721     C                   ELSE
107200030721     C                   SETON                                        32
107300030721     C                   EVAL      VABCAS = *zeros
107400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107500030721     C                             + ' ' + 'VABCAS'
107600030721     C                   ENDIF
107700030721     C                   ENDIF
107800030721     C*** CCM
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
108000030721     C                   EVAL      PiStr=SkSplitCSV(i)
108100030721     C                   EXSR      CHKNUM
108200030721     C                   IF        PiInt=*on      AND
108300030721     C                             PiVal<=9999999 AND
108400030721     C                             PiVal>*zeros
108500030721     C                   Z-ADD     PiVal         VABCCM
108600030721     C                   ELSE
108700030721     C                   SETON                                        32
108800030721     C                   EVAL      VABCCM = *zeros
108900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109000030721     C                             + ' ' + 'VABCCM'
109100030721     C                   ENDIF
109200030721     C                   ENDIF
109300030721     C*** LNP
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
109500030721     C                   EVAL      PiStr=SkSplitCSV(i)
109600030721     C                   EXSR      CHKNUM
109700030721     C                   IF        PiInt=*on    AND
109800030721     C                             PiVal<=999   AND
109900030721     C                             PiVal>*zeros
110000030721     C                   Z-ADD     PiVal         VABLNP
110100030721     C                   ELSE
110200030721     C                   SETON                                        32
110300030721     C                   EVAL      VABLNP = *zeros
110400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110500030721     C                             + ' ' + 'VABLNP'
110600030721     C                   ENDIF
110700030721     C                   ENDIF
110800030721     C*** NRS
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
111000030721     C                   EVAL      PiStr=SkSplitCSV(i)
111100030721     C                   EXSR      CHKNUM
111200030721     C                   IF        PiInt=*on AND
111300030721     C                             PiVal<=99
111400030721     C                   Z-ADD     PiVal         VABNRS
111500030721     C                   ELSE
111600030721     C                   SETON                                        32
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800030721     C                             + ' ' + 'VABNRS'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** LNA
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiInt=*on    AND
112600030721     C                             PiVal<=999   AND
112700030721     C                             PiVal>*zeros
112800030721     C                   Z-ADD     PiVal         VABLNA
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      VABLNA = *zeros
113200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113300030721     C                             + ' ' + 'VABLNA'
113400030721     C                   ENDIF
113500030721     C                   ENDIF
113600030721     C*** CTR
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
113800030721     C                   EVAL      PiStr=SkSplitCSV(i)
113900030721     C                   EXSR      CHKNUM
114000030721     C                   IF        PiInt=*on  AND
114100030721     C                             PiVal<=999
114200030721     C                   Z-ADD     PiVal         VABCTR
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600030721     C                             + ' ' + 'VABCTR'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900030721     C*** IAS
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
115100030721     C                   EVAL      PiStr=SkSplitCSV(i)
115200050623     C     X'20':' '     XLATE     PiStr         PiStr
115300050623     C                   EVAL      PiStr = %trim(PiStr)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiNum=*on
115600030721     C                   Z-ADD     PiVal         VABIAS
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116000030721     C                             + ' ' + 'VABIAS'
116100030721     C                   ENDIF
116200030721     C                   ENDIF
116300030721     C*** VLB
116400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700030721     C                   IF        PiNum=*on
116800030721     C                   Z-ADD     PiVal         VABVLB
116900030721     C                   ELSE
117000030721     C                   SETON                                        32
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200030721     C                             + ' ' + 'VABVLB'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** QFT
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900030721     C                   IF        PiNum=*on
118000030721     C                   Z-ADD     PiVal         VABQFT
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABQFT'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** NCD
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on      AND
119200030721     C                             PiVal<=9999999
119300030721     C                   Z-ADD     PiVal         VABNCD
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700030721     C                             + ' ' + 'VABNCD'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030721     C*** NCA
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiInt=*on      AND
120500030721     C                             PiVal<=9999999
120600030721     C                   Z-ADD     PiVal         VABNCA
120700030721     C                   ELSE
120800030721     C                   SETON                                        32
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABNCA'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** ZNC
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on AND
121800030721     C                             PiVal<=99
121900030721     C                   Z-ADD     PiVal         VABZNC
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABZNC'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** DCR
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on       AND
123100030721     C                             PiVal<=99999999
123200030721     C                   Z-ADD     PiVal         VABDCR
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABDCR'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** HCR
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on   AND
124400030721     C                             PiVal<=9999
124500030721     C                   Z-ADD     PiVal         VABHCR
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABHCR'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** VMD
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiNum=*on
125700030721     C                   Z-ADD     PiVal         VABVMD
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABVMD'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** ANT
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiInt=*on        AND
126900030721     C                             PiVal<=999999999
127000030721     C                   Z-ADD     PiVal         VABANT
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABANT'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700030721     C*** CMO
127800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiInt=*on
128200030721     C                   Z-ADD     PiVal         Num5_0
128300030721     C                   MOVEL(p)  Num5_0        VABCMO
128400030721     C                   ELSE
128500030721     C                   SETON                                        32
128600030721     C                   EVAL      VABCMO = *zeros
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABCMO'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030715     C*
129200030715     C                   ENDIF
129300030715     C                   EVAL      i = i + 1
129400030715     C                   ENDDO
129500040726     C*
129600040726     C* Considerazioni personalizzate x il traduttore corrente
129700050506     C                   MOVE(P)   VABRMN        VABNSP
129800040726     C*
129900040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
130000040726     C                   IF        VABNSP = *zeros
130100040726     C                   SETON                                        31
130200040726     C                   ENDIF
130300040726     C*
130400040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
130500040726     C                   IF        VABLNP = *zeros
130600040726     C                   SETON                                        31
130700040726     C                   ENDIF
130800050623     C*
130900050623     C* Se CCM nn valorizzato considero errore bloccante in traduzione
131000050623     C                   IF        VABCCM = *zeros
131100050623     C                   SETON                                        31
131200050623     C                   ENDIF
131300040726     C*
131400040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
131500040726     C* d valorizzarli sempre entrambi
131600040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
131700040726     C                   Z-ADD     VABNSP        VABRMN
131800040726     C                   ENDIF
131900040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
132000040726     C                   MOVEL     VABRMN        VABRMA
132100040726     C                   ENDIF
132200040726     C*
132300040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
132400040726     C                   IF        VABCAS > *zeros
132500040726     C                   IF        VABVCA = *blanks
132600040726     C                   EVAL      VABVCA = 'EUR'
132700040726     C                   ENDIF
132800040726     C                   IF        VABCBO = '1'
132900040726     C                   EVAL      VABCBO = '4'
133000040726     C                   ENDIF
133100040726     C                   IF        VABCBO = '2'
133200040726     C                   EVAL      VABCBO = '6'
133300040726     C                   ENDIF
133400040726     C                   ENDIF
133500020204     C*
133600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
133700020204     C                   EXSR      CHKIMPDIV
133800030715     C                   ENDIF
133900020204     C*
134000010607     C                   ENDSR
134100010607     C*----------------------------------------------------*
134200010601
134300020204
134400020204     C*----------------------------------------------------*
134500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
134600020204     C*----------------------------------------------------*
134700020204     C     CHKIMPDIV     BEGSR
134800020204     C*
134900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
135000020204     C                   Z-ADD     *zeros        wrkDec            9 9
135100020204     C*
135200020204     C* Come prima cosa effettuo considerazioni sulla divisa
135300020204     C                   IF        vabIAS > *zeros
135400020204     C                   IF        vabVAS <> 'EUR'
135500020204     C                   EVAL      vabVAS =  'ITL'
135600020204     C                   ENDIF
135700020204     C                   ENDIF
135800020204     C*
135900020204     C                   IF        vabCAS > *zeros
136000020204     C                   IF        vabVCA <> 'EUR'
136100020204     C                   EVAL      vabVCA =  'ITL'
136200020204     C                   ENDIF
136300020204     C                   ENDIF
136400020204     C*
136500020204     C                   IF        vabVMD > *zeros
136600020204     C                   IF        vabVAD <> 'EUR'
136700020204     C                   EVAL      vabVAD =  'ITL'
136800020204     C                   ENDIF
136900020204     C                   ENDIF
137000020204     C*
137100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
137200020204     C                   Z-ADD     vabIAS        wrkDec
137300020204     C                   IF        wrkDec > *zeros
137400020204     C                   IF        vabVAS = 'ITL'
137500020204     C                   EVAL      vabIAS = *zeros
137600020204     C                   ENDIF
137700020204     C                   ENDIF
137800020204     C*
137900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
138000020204     C                   Z-ADD     vabCAS        wrkDec
138100020204     C                   IF        wrkDec > *zeros
138200020204     C                   IF        vabVCA = 'ITL'
138300020204     C                   EVAL      vabCAS = *zeros
138400020204     C                   ENDIF
138500020204     C                   ENDIF
138600020204     C*
138700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
138800020204     C                   Z-ADD     vabVMD        wrkDec
138900020204     C                   IF        wrkDec > *zeros
139000020204     C                   IF        vabVAD = 'ITL'
139100020204     C                   EVAL      vabVMD = *zeros
139200020204     C                   ENDIF
139300020204     C                   ENDIF
139400020204     C*
139500020204     C                   ENDSR
139600020204     C***
139700020204
139800010330
139900010330
140000010330     C*----------------------------------------------------*
140100010330     C*  CONTROLLO NUMERICITA' CAMPI
140200010330     C*----------------------------------------------------*
140300010330     C     CHKNUM        BEGSR
140400010330     C*
140500010606     C                   IF        PiDecChr = *blanks
140600030715     C                   EVAL      PiDecChr = CharNUM
140700010606     C                   ENDIF
140800010606     C*
140900010606     C                   CALL(e)   'ISNUMERIC'
141000010330     C                   PARM                    PiStr            30
141100010606     C                   PARM                    PiDecChr          1
141200010330     C                   PARM      *ZEROS        PiVal            30 9
141300010330     C                   PARM      '0'           PiInt             1
141400010330     C                   PARM      '0'           PiNum             1
141500010330     C                   IF        %error
141600010606     C                   EVAL      PiNum=*off
141700010330     C                   ENDIF
141800010330     C*
141900010330     C                   ENDSR
142000010330     C***
142100010330
142200010601
142300010601
142400010601
142500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
142600010601     C     repfil        BEGSR
142700010601     C*
142800010601     C                   if        invfil = *zeros and
142900010601     C                             depfil > *zeros and
143000010629     C                             (vinflg = *blanks or
143100010629     C                              vinflg = *zeros)
143200010601     C
143300010601     C                   eval      invfil = depfil
143400010601     C                   endif
143500010601     C*
143600010601     C                   if        depfil <> invfil and
143700010601     C                             invfil > *zeros
143800010601     C                   eval      flgMulti = '1'
143900010601     C                   if        vinflg = *blanks
144000010601     C                   add       1             cntNonEl
144100010601     C                   endif
144200010601     C                   endif
144300010601     C*
144400010601     C                   if        vinflg = '2'
144500010601     C                   eval      flgStato = '2'
144600010601     C                   endif
144700010601     C*
144800010601     C                   ENDSR
144900010601     C***
145000010601
145100010601
145200010601
145300010330
145400010330
145500010330
145600990920      /TITLE Invio dei dati al punto operativo.
145700000613     C     invio         BEGSR
145800050722     C*
145900050722     C* 1° invio FIVAT
146000050722     C                   reset                   dscmz
146100050722     C                   move      vlrpoi        cmzdst
146200050722     C                   eval      cmzfld = 'FIVATWWR'
146300050722     C                   eval      cmzmbd = vlrhdl
146400050722     C                   eval      %subst(cmzmbd:1:1) = 'M'
146500050722     C***                if        prmfir = *blanks
146600050722     C                   eval      cmzfla = 'FIVAT00F'
146700050722     C                   eval      cmzmba = 'FIVAT00F'
146800050722     C***                else
146900050722     C***                eval      cmzfla = prmfir
147000050722     C***                eval      cmzmba = prmfir
147100050722     C***                endif
147200050722     C                   eval      cmznrr = *zeros
147300050722     C                   move      §ctrokvt      cmznrr
147400050722     C                   eval      cmzlba = vlrfl1
147500050722     C                   call(e)   'TIS711C'
147600050722     C                   parm                    dscmz
147700050722     C                   parm      *blanks       esito
147800050722     C                   if        %error
147900050722     C                             or cmzerr = '1'
148000050722     C                             or esito  = '1'
148100050722     C                   eval      wrkesito = '3'
148200050722     C                   else
148300050722     C*
148400050722     C* 2° invio FIVAB
148500050722     C                   reset                   dscmz
148600050722     C                   move      vlrpoi        cmzdst
148700050722     C                   eval      cmzfld = vlrfou
148800050722     C                   eval      cmzmbd = vlrhdl
148900050722     C                   eval      %subst(cmzmbd:1:1) = 'M'
149000050722     C***                if        prmfir = *blanks
149100050722     C                   eval      cmzfla = 'FIVAB00F'
149200050722     C                   eval      cmzmba = 'FIVAB00F'
149300050722     C***                else
149400050722     C***                eval      cmzfla = prmfir
149500050722     C***                eval      cmzmba = prmfir
149600050722     C***                endif
149700050722     C                   eval      cmznrr = *zeros
149800050722     C                   move      §ctrokvb      cmznrr
149900050722     C                   eval      cmzlba = vlrfl1
150000050722     C                   call(e)   'TIS711C'
150100050722     C                   parm                    dscmz
150200050722     C                   parm      *blanks       esito
150300050722     C                   if        %error
150400050722     C                             or cmzerr = '1'
150500050722     C                             or esito  = '1'
150600050722     C                   eval      wrkesito = '3'
150700050722     C                   endif
150800050722     C                   endif
150900990920     C*
151000000613     C                   ENDSR
151100990910
151200010601
151300010601
151400010601
151500010601
151600010601      /TITLE Invio dei dati al punto operativo.
151700010601     C     opeini        BEGSR
151800010601     C*
151900010601     C* Inizializzo flag e contatori operativi
152000010601     C                   movel     '0'           flgGiro           1
152100010601     C                   movel     '0'           flgMulti          1
152200010601     C                   movel     '1'           flgStato          1
152300010615     C                   movel     '0'           flgOk             1
152400010601     C                   z-add     *zeros        cntNonEl         10 0
152500010601     C                   z-add     *zeros        depfil            3 0
152600010601     C                   z-add     *zeros        invfil            3 0
152700010601     C*
152800010601     C                   ENDSR
152900010601     C***
153000010601
153100010601
153200050421
153300050421
153400070411
153500070411     C     *pssr         BEGSR
153600070411     C*
153700070411     C                   if        %open(tivin00r)
153800070411     C                   close     tivin00r
153900070411     C                   endif
154000070411     C                   if        %open(fivabwwr)
154100070411     C                   close     fivabwwr
154200070411     C                   endif
154300070411     C                   if        %open(fivatwwr)
154400070411     C                   close     fivatwwr
154500070411     C                   endif
154600070411     C*
154700070411     C* Effettuo la chiamata al CLLE preposto
154800070411     C                   call(e)   'TITVVTC'
154900070411     C                   parm                    parccm
155000070411     C                   parm                    parmbr
155100070411     C                   parm      '2'           paropz
155200070411     C*
155300070411     C                   eval      wrkesito = '2'
155400070411     C*
155500070411     C                   seton                                        LR
155600070411     C*
155700070411     C                   ENDSR     '*CANCL'
155800070411     C***
155900050421
156000010601
156100010330
156200010330
156300000613     C     *inzsr        BEGSR
156400990910     C*
156500990910     C     *entry        plist
156600990920     C                   parm                    tivlrds
156700990921     C                   parm      wrkesito      esito
156800000724     C                   parm                    prmlit
156900000710     C                   parm                    prmfir
157000050505     C*
157100050505     C* CALCOLA LA DATA CORRENTE
157200050505     C                   time                    wn14             14 0
157300050505     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
157400050505     C                   z-add     wn8           g08dat
157500050505     C                   z-add     *zeros        g08inv
157600050505     C                   movel     '0'           g08err
157700050505     C                   call      'XSRDA8'
157800050505     C                   parm                    wlbda8
157900050505     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
158000000613     C*
158100000613     C                   ENDSR
158200000613     C***
158300990908
158400030715     Otitvxlp   E            riepilogo         2
158500990915     O                                              'Upload via Internet'
158600990915     O                                           +1 'Traduzione TIVIN00R -'
158700021025     O                                           +1 'FIVABWWR'
158800990915     O                                           +1 'Testate bolle'
158900990915     O          E            riepilogo   2
159000990915     O                       wrkdata
159100990915     O                       wrkora              +1
159200990915     O                       procname            +1
159300990915     O          E            riepilogo   2
159400990915     O                                              'Cliente..................:'
159500010605     O                       VABCCM        z     +1
159600990915     O          E            riepilogo   2
159700990920     O                                              'Riferimento Strategi.....:'
159800990920     O                       vlrhdl              +1
159900990915     O          E            riepilogo   2
160000990915     O                                              'Giusti...................:'
160100050722     O                       §CTROKVB      2   +  1
160200990915     O          E            riepilogo   2
160300990915     O                                              'Sbagliati e corretti.....:'
160400971022     O                       §CTRMO        2   +  1
160500990915     O          E            riepilogo   2
160600990915     O                                              'Sbagliati e scartati.....:'
160700971022     O                       §CTRNO        2   +  1
160800000613
160900030715     Otitvxlps  E            testdett          2
161000000613     O                                              'Upload via Internet'
161100000613     O                                           +1 'Traduzione TIVIN00R -'
161200021025     O                                           +1 'FIVABWWR'
161300000613     O                                           +1 'Testate bolle'
161400000616     O          E            testdett    3
161500000613     O                                           +2 'N° rec'
161600000613     O                                           +3 'Anteprima contenuto'
161700000616     O          E            rigadett    2
161800000613     O                       rrnum               +2
161900000621     O                       recko               +3
162000000616     O          E            findett     2
162100000613     O                       wrkdata
162200000613     O                       wrkora              +1
162300000613     O                       procname            +1
162400000616     O          E            findett     2
162500000613     O                                              'Cliente..................:'
162600010605     O                       VABCCM        z     +1
162700000616     O          E            findett     2
162800000613     O                                              'Riferimento Strategi.....:'
162900000613     O                       vlrhdl              +1
163000000616     O          E            findett     2
163100000613     O                                              'Giusti...................:'
163200050722     O                       §CTROKVB      2   +  1
163300000616     O          E            findett     2
163400000613     O                                              'Sbagliati e corretti.....:'
163500000613     O                       §CTRMO        2   +  1
163600000616     O          E            findett     2
163700000613     O                                              'Sbagliati e scartati.....:'
163800000613     O                       §CTRNO        2   +  1
163900000613** CMD - COMANDI CL
164000030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
164100030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
164200030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
164300010606
164400000613
164500050505** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
164600050623VABRMN;VABCBO;VABRSD;VABIND;VABCAD;VABLOD;
164700050722VABPRD;VATNOT;VABNCL;VABPKB;VABIAS;VABRMA
164800050505
164900050505
165000050505
165100050505
165200050505
165300050505
165400050505
165500050505
