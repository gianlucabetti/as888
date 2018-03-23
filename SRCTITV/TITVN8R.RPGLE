000100050811      /TITLE Upload via Internet: traduzione in FIVAB/FIVAT. (Cliente NEBUS - Cod. 0433035)
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
002900050811     D tivlrds       e ds                  extname(tivlr00f)
003000050811     D tisi95ds      e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000050722     D parccm          s              8    INZ(*blanks)
004100050722     D parmbr          s             10    INZ(*blanks)
004200050722     D paropz          s              1    INZ(*blanks)
004300050722     D chkcall         s              1    INZ(*blanks)
004400040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004500030715     D*------------------
004600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004700030715     D*------------------
004800030715     D SkSplitFLD      S             10    DIM(100)
004900030715     D SkSplitCSV      S            256    DIM(100)
005000050811     D SkVAT           S             36    DIM(10) DESCEND
005100050811     D jVAT            s              2  0 INZ(*zeros)
005200030715     D CharCSV         S              1
005300030715     D CharTXT         S              1
005400030715     D CharNUM         S              1
005500030715     D posDa           S              3  0 INZ(*zeros)
005600030715     D posA            S              3  0 INZ(*zeros)
005700030715     D i               s              3  0 INZ(1)
005800030715     D wGiro           s              1  0 INZ(*zeros)
005900050811     D*------------------
006000050811     D* DS X RIDEFINIZIONE CAMPI NECESSARI ALLA MEMORIZZAZIONE DEI TIPI RECORD X VAT
006100050811     D*------------------
006200050811     D wDSVAT          DS                  INZ
006300050811     D  wDSVAT_NOT                   35
006400050811     D  wDSVAT_TRC                    1
006500030822     D*------------------
006600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006700030822     D*------------------
006800030822     D posDaDft        S              3  0 INZ(*zeros)
006900030822     D posADft         S              3  0 INZ(*zeros)
007000030822     D j               s              3  0 INZ(1)
007100030822     D wGiroDft        s              1  0 INZ(*zeros)
007200010330     D*------------------
007300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007400010330     D*------------------
007500010330     D WLBDA8          DS                  INZ
007600010330     D  G08DAT                 1      8  0
007700010330     D  G08INV                 9     16  0
007800010330     D  G08ERR                17     17
007900010330     D  G08TGI                18     22  0
008000050811     D*------------------
008100050811     D* DS REPERIMENTO NUMERATORE
008200050811     D*------------------
008300050811     D trul33ds      e ds                  inz
008400050811     D*------------------
008500050811     D* DS ARCHITETTURA
008600050811     D*------------------
008700050811     D kpjba         e ds                  inz
008800031201     D*------------------
008900031201     D* Costanti
009000031201     D*------------------
009100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009300010330
009400010330
009500010330
009600990915     C                   time                    wrkdata
009700990915     C                   time                    wrkora
009800000913     C                   reset                   rrnum
009900990921     C                   reset                   esito
010000990921     C                   reset                   wrkesito
010100010601     C*
010200010601     C                   exsr      opeini
010300010605     C                   exsr      rwvab
010400050811     C*
010500050811     C* Esegue lancio TISI95R solo x chiusura
010600050811     C                   CLEAR                   TISI95DS
010700050811     C                   EVAL      I95TLA = 'C'
010800050811     C                   CALL      'TISI95R'
010900050811     C                   PARM                    TISI95DS
011000010601     C*
011100010601     C                   seton                                        lr
011200010601
011300010601
011400010601
011500010601
011600010601     C*--------------------------------------------------------
011700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011800010601     C*--------------------------------------------------------
011900010601     C     PREELA        BEGSR
012000010601     C*
012100010601     C* SE OCCORRE SPEDIRE IN FILIALE
012200010601     C                   if        invfil <> *zeros and
012300010601     C                             flgGiro = '0'
012400010601     C*
012500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012600010601     C                   eval      flgGiro = '1'
012700000724     C*
012800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013000010601     C     invfil        chain     azorg01l
013100000724     C                   if        %found
013200000724     C                   movel(p)  CMD(1)        depcmd
013300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013400000724     C*
013500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013600011113     C                   Z-ADD     150           LENGH            15 5
013700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013800000724     C                   PARM                    depcmd
013900000724     C                   PARM                    LENGH
014000000724     C*
014100000724     C                   endif
014200000616     C*
014300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014400000616     C                   movel(p)  CMD(2)        depcmd
014500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014600000616     C*
014700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014800011113     C                   Z-ADD     150           LENGH            15 5
014900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015000000616     C                   PARM                    depcmd
015100000616     C                   PARM                    LENGH
015200000613     C*
015300030715     C                   if        not %open(titvxlps)
015400030715     C                   open      titvxlps
015500030715     C                   except    testdett
015600000613     C                   endif
015700010601     C*
015800010601     C                   endif
015900010601     C*
016000010601     C                   ENDSR
016100010601     C***
016200010601
016300010601
016400010601
016500010601
016600010601
016700010601
016800010601
016900010601     C*--------------------------------------------------------
017000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017100010601     C*--------------------------------------------------------
017200010601     C     ENDELA        BEGSR
017300990915     C*
017400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017500050722     C*
017600050722     C* Effettuo la chiamata al CLLE preposto
017700050722     C                   call(e)   'TITVVTC'
017800050722     C                   parm                    parccm
017900050722     C                   parm                    parmbr
018000050722     C                   parm      '2'           paropz
018100000613     C*
018200030715     C                   if        %open(titvxlps)
018300000616     C                   except    findett
018400030715     C                   close     titvxlps
018500000613     C                   endif
018600000616     C*
018700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018800011113     C                   Z-ADD     150           LENGH            15 5
018900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019000000616     C                   PARM                    CMD(3)
019100000616     C                   PARM                    LENGH
019200000616     C*
019300010601     C                   ENDSR
019400010601     C***
019500010601
019600010601
019700010601
019800010601
019900000613
020000000613     C*--------------------------------------------------------
020100000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020200000613     C*--------------------------------------------------------
020300000621     C     STPR          BEGSR
020400000613     C*
020500030715     C                   if        not %open(titvxlp)
020600030715     C   50              open      titvxlp
020700990915     C                   endif
020800990915     C*
020900030715     C   50              except    riepilogo
021000990915     C*
021100030715     C                   if        %open(titvxlp)
021200030715     C   50              close     titvxlp
021300990914     C                   endif
021400000613     C*
021500000613     C                   ENDSR
021600000613     C***
021700000613
021800010330
021900010601
022000010601
022100010601
022200010330     C*--------------------------------------------------------
022300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022400010330     C*--------------------------------------------------------
022500010605     C     RWVAB         BEGSR
022600010330     C*
022700010330     C                   if        not %open(tivin00r)
022800010330     C                   open      tivin00r
022900010330     C                   endif
023000021025     C                   if        not %open(fivabwwr)
023100021025     C                   open      fivabwwr
023200010330     C                   endif
023300050722     C*
023400050722     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023500050722     C                   exsr      prevat
023600050722     C*
023700050722     C                   if        chkcall = '0'
023800050722     C*
023900050722     C                   if        not %open(fivatwwr)
024000050722     C                   open      fivatwwr
024100050722     C                   endif
024200010330     C*
024300050722     C                   clear                   §CTROKVB          7 0
024400050722     C                   clear                   §CTROKVT          7 0
024500050722     C                   clear                   §CTRMO            7 0
024600050722     C                   clear                   §CTRNO            7 0
024700010330     C*
024800030822     C                   do        *HIVAL
024900010330     C*
025000030822     C                   read      tivin00r                               70
025100010618     C*
025200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025300010618     C                   if        vinflg = '1'
025400010618     C                   eval      flgOk = '1'
025500010618     C                   endif
025600010618     C*
025700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025800040510     C                   z-add     *zeros        wLenVINDTA        4 0
025900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026300040510     C                   z-add     1             wFlgVINDTA
026400040510     C                   leave
026500040510     C                   endif
026600040510     C                   enddo
026700040510     C*
026800040218     C                   if        vindta > *blanks AND
026900040510     C                             wFlgVINDTA = 1
027000010330     C                   add       1             rrnum
027100010330     C*
027200010601     C                   if        *in70 = *off and
027300010330     C                             (vinflg = *blanks
027400010330     C                              or vinflg = '0'
027500010330     C                              or vinflg = '2')
027600010330     C*
027700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027800010711     C                   if        vinflg = *blanks or vinflg = '0'
027900010711     C                   clear                   vinmsg
028000010711     C                   endif
028100010601     C*
028200010330     C                   exsr      inzvar
028300010605     C                   exsr      impvab
028400010601     C*
028500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028600010605     C                   eval      depfil = VABLNP
028700010601     C                   exsr      repfil
028800010601     C                   if        depfil = invfil
028900021025     C                   if        vlrpoi = 999
029000030822     C                   move(P)   invfil        VABFGS
029100050722     C                   move(P)   invfil        VATFGS
029200021025     C                   else
029300030822     C                   move(P)   vlrpoi        VABFGS
029400050722     C                   move(P)   vlrpoi        VATFGS
029500021025     C                   endif
029600010601     C*
029700010601     C                   exsr      PREELA
029800010601     C*
029900010604     C* Ebbene...
030000010604     C*
030100050722     C  N31              add       1             §CTROKVB
030200050722     C   32              add       1             §CTRMO
030300050722     C   31              add       1             §CTRNO
030400030822     C                   if        wGiro = 2
030500030822     C  N31              write     FIVAB000
030600050722     C  N31              exsr      wrivat
030700030822     C                   endif
030800010604     C*
030900010604     C                   if        *in31 = *off and
031000010604     C                             *in32 = *off
031100010604     C                   eval      vinflg = '1'
031200010604     C                   else
031300010604     C                   eval      recko = vindta
031400010604     C                   except    rigadett
031500010604     C                   eval      vinflg = '2'
031600010604     C                   endif
031700010604     C                   endif
031800010601     C*
031900010604     C                   endif
032000010604     C*
032100010330     C                   else
032200010330     C                   eval      vinflg = '1'
032300010330     C                   endif
032400010601     C*
032500010601     C  N70              update    tivin000
032600010330     C*
032700030822     C  N70              enddo
032800050722     C*
032900050722     C                   endif                                                  (endif - chkcall)
033000010601     C*
033100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033200010601     C                   if        cntNonEl = *zeros or
033300010601     C                             flgMulti = '0'
033400010330     C* Se non ci sono record con errori ...
033500010601     C                   if        §ctrno = 0 and
033600010604     C                             §ctrmo = 0 and
033700010601     C                             flgStato <> '2'
033800010330     C* ... restituisco esito OK.
033900010330     C                   eval      wrkesito = '0'
034000010330     C                   else
034100050722     C                   if        §ctrokvb > 0
034200010330     C                   eval      wrkesito = '1'
034300010330     C                   else
034400010615     C                   if        flgOk = '0'
034500010615     C                   eval      wrkesito = '2'
034600010615     C                   else
034700010615     C                   eval      wrkesito = '6'
034800010615     C                   endif
034900010330     C                   endif
035000010330     C                   endif
035100010601     C                   else
035200010601     C                   eval      wrkesito = '9'
035300010601     C                   endif
035400010330     C*
035500010330     C                   if        %open(tivin00r)
035600010330     C                   close     tivin00r
035700010330     C                   endif
035800021025     C                   if        %open(fivabwwr)
035900021025     C                   close     fivabwwr
036000010330     C                   endif
036100050722     C                   if        %open(fivatwwr)
036200050722     C                   close     fivatwwr
036300050722     C                   endif
036400010601     C*
036500010601     C                   if        vlrpoi <> 999
036600010601     C                   eval      invfil = vlrpoi
036700010601     C                   endif
036800010330     C*
036900050722     C                   if        §ctrokvb > 0
037000010601     C                             and invfil > *zeros
037100010330     C                   exsr      invio
037200010330     C                   endif
037300010601     C*
037400010618     C                   if        flgGiro = '1'
037500010601     C                   exsr      endela
037600010618     C                   endif
037700010330     C*
037800010330     C                   ENDSR
037900010330     C***
038000050722
038100050722
038200050722
038300050722     C*----------------------------------------------------*
038400050722     C*  SCARICAMENTO BUFFER RECORDS VAT
038500050722     C*----------------------------------------------------*
038600050722     C     WRIVAT        BEGSR
038700050722     C*
038800050722     C* Attribuisco la stessa chiave della testata bolle
038900050722     C                   EVAL      VATAAS = VABAAS
039000050722     C                   EVAL      VATLNP = VABLNP
039100050722     C                   EVAL      VATNRS = VABNRS
039200050722     C                   EVAL      VATNSP = VABNSP
039300050722     C                   EVAL      VATCCM = VABCCM
039400050811     C*
039500050811     C* CICLO SU TUTTI GLI ELEMENTI DELLA SCHIERA MEMORIZZAZIONE TIPI RECORD VAT MEMORIZZATI
039600050811     C                   SORTA     SkVAT
039700050811     C                   Z-ADD     1             jVAT
039800050811     C                   DOW       jVAT <= %elem(SkVAT)
039900050811     C                   EVAL      wDSVAT = SkVAT(jVAT)
040000050722     C*
040100050722     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
040200050811     C                   IF        wDSVAT_NOT <> *blanks
040300050811     C                   EVAL      VATNOT = wDSVAT_NOT
040400050811     C                   EVAL      VATTRC = wDSVAT_TRC
040500050722     C                   WRITE     FIVAT000
040600050722     C                   ADD       1             §CTROKVT
040700050811     C                   ELSE
040800050811     C                   LEAVE
040900050722     C                   ENDIF
041000050811     C*
041100050811     C                   ADD       1             jVAT
041200050811     C                   ENDDO
041300050722     C*
041400050811     C                   Z-ADD     *zeros        jVAT
041500050811     C*
041600050722     C                   ENDSR
041700050722
041800050722
041900050722
042000050722     C*----------------------------------------------------*
042100050722     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042200050722     C*----------------------------------------------------*
042300050722     C     PREVAT        BEGSR
042400050722     C*
042500050722     C* Compongo il nome del membro da dare al FIVATWWR
042600050722     C                   eval      parmbr = vlrhdl
042700050722     C                   movel     'M'           parmbr
042800050722     C                   eval      parccm = vlrksc
042900050722     C                   eval      paropz = '1'
043000050722     C* Effettuo la chiamata al CLLE preposto
043100050722     C                   call(e)   'TITVVTC'
043200050722     C                   parm                    parccm
043300050722     C                   parm                    parmbr
043400050722     C                   parm                    paropz
043500050722     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043600050722     C                   if        %error
043700050722     C                   movel     '1'           chkcall
043800050722     C                   else
043900050722     C                   movel     '0'           chkcall
044000050722     C                   endif
044100050722     C*
044200050722     C                   ENDSR
044300010601
044400010601
044500010601
044600010330     C*----------------------------------------------------*
044700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
044800010330     C*----------------------------------------------------*
044900010330     C     INZVAR        BEGSR
045000010330     C*
045100030822     C* Inizializzo variabili di wrk
045200020204     C                   Z-ADD     *zeros        Num5_0            5 0
045300030822     C*
045400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
045500030822     C                   CLEAR                   FIVAB000
045600050722     C                   CLEAR                   FIVAT000
045700030822     C                   CLEAR                   SkSplitCSV
045800050811     C                   CLEAR                   SkVAT
045900050811     C                   EVAL      jVAT = *zeros
046000030822     C                   EVAL      i = 1
046100030822     C                   EVAL      posDa = *zeros
046200030822     C                   EVAL      posA  = *zeros
046300030822     C*
046400030822     C* Reimposto i valori di default
046500030822     C                   EXSR      DEFCAM
046600030822     C*
046700010330     C                   ENDSR
046800010330     C*----------------------------------------------------*
046900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
047000010330     C*----------------------------------------------------*
047100010330     C     DEFCAM        BEGSR
047200030822     C*
047300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
047400030715     C* e delimitatore testo.
047500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
047600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
047700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
047800030822     C*
047900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
048000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
048100030715     C                   SETON                                        50
048200030715     C                   ELSE
048300030715     C                   SETOFF                                       50
048400030715     C                   ENDIF
048500030822     C*
048600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
048700030822     C                   EVAL      posDaDft = 1
048800030822     C                   EVAL      posADft  = 0
048900030822     C                   EVAL      wGiroDft = 0
049000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
049100030822     C                             posDaDft > 0
049200030822     C*
049300030822     C* Gestisco il 1° giro
049400030822     C                   IF        wGiroDft = 0
049500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
049600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
049700030822     C* Incremento il contatore dei "giri"
049800030822     C                   EVAL      wGiroDft = 1
049900030822     C                   ELSE
050000030822     C                   EVAL      posDaDft = posADft
050100030822     C                   ENDIF
050200030822     C* Eseguo lo scan x trovare la fine del campo corrente
050300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
050400030822     C*
050500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
050600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
050700030822     C                   IF        posDaDft > 0 AND
050800030822     C                             posADft  > 0
050900030822     C* NCL
051000030822     C                   IF        %subst(
051100030822     C                             %subst(vlrppt:posDaDft+1:
051200030822     C                             posADft-posDaDft-1):1:3)
051300030822     C                             = 'NCL'
051400030822     C                   EVAL      PiStr=%trim(%subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051700030822     C                   EXSR      CHKNUM
051800030822     C                   IF        PiInt=*on
051900030822     C                   Z-ADD     PiVal         VABNCL
052000030822     C                   ENDIF
052100030822     C                   ENDIF
052200030822     C* CCM
052300030822     C                   IF        %subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):1:3)
052600030822     C                             = 'CCM'
052700030822     C                   EVAL      PiStr=%trim(%subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053000030822     C                   EXSR      CHKNUM
053100030822     C                   IF        PiInt=*on
053200030822     C                   Z-ADD     PiVal         VABCCM
053300030822     C                   ENDIF
053400030822     C                   ENDIF
053500030822     C* LNP
053600030822     C                   IF        %subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):1:3)
053900030822     C                             = 'LNP'
054000030822     C                   EVAL      PiStr=%trim(%subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300030822     C                   EXSR      CHKNUM
054400030822     C                   IF        PiInt=*on
054500030822     C                   Z-ADD     PiVal         VABLNP
054600030822     C                   ENDIF
054700030822     C                   ENDIF
054800030822     C* NRS
054900030822     C                   IF        %subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):1:3)
055200030822     C                             = 'NRS'
055300030822     C                   EVAL      PiStr=%trim(%subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600030822     C                   EXSR      CHKNUM
055700030822     C                   IF        PiInt=*on
055800030822     C                   Z-ADD     PiVal         VABNRS
055900030822     C                   ENDIF
056000030822     C                   ENDIF
056100030822     C* CTR
056200030822     C                   IF        %subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):1:3)
056500030822     C                             = 'CTR'
056600030822     C                   EVAL      PiStr=%trim(%subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900030822     C                   EXSR      CHKNUM
057000030822     C                   IF        PiInt=*on
057100030822     C                   Z-ADD     PiVal         VABCTR
057200030822     C                   ENDIF
057300030822     C                   ENDIF
057400030822     C* PKB
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'PKB'
057900030822     C                   EVAL      PiStr=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   EXSR      CHKNUM
058300030822     C                   IF        PiNum=*on
058400051121     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a Kg.
058500030822     C                   Z-ADD     PiVal         VABPKB
058600030822     C                   ENDIF
058700030822     C                   ENDIF
058800030822     C* VLB
058900030822     C                   IF        %subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):1:3)
059200030822     C                             = 'VLB'
059300030822     C                   EVAL      PiStr=%trim(%subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600030822     C                   EXSR      CHKNUM
059700030822     C                   IF        PiNum=*on
059800030822     C                   Z-ADD     PiVal         VABVLB
059900030822     C                   ENDIF
060000030822     C                   ENDIF
060100030822     C* QFT
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'QFT'
060600030822     C                   EVAL      PiStr=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   EXSR      CHKNUM
061000030822     C                   IF        PiNum=*on
061100030822     C                   Z-ADD     PiVal         VABQFT
061200030822     C                   ENDIF
061300030822     C                   ENDIF
061400030822     C* CBO
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'CBO'
061900030822     C                   EVAL      VABCBO=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   ENDIF
062300030822     C* TSP
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'TSP'
062800030822     C                   EVAL      VABTSP=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   ENDIF
063200030822     C* VAS
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'VAS'
063700030822     C                   EVAL      VABVAS=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   ENDIF
064100030822     C* VCA
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'VCA'
064600030822     C                   EVAL      VABVCA=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   ENDIF
065000030822     C* GCA
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'GCA'
065500030822     C                   EVAL      VABGCA=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* CTM
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'CTM'
066400030822     C                   EVAL      VABCTM=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800030822     C* FFD
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'FFD'
067300030822     C                   EVAL      VABFFD=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   ENDIF
067700030822     C* VAD
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'VAD'
068200030822     C                   EVAL      VABVAD=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   ENDIF
068600030822     C* GMA
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'GMA'
069100030822     C                   EVAL      VABGMA=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   ENDIF
069500030822     C* GGA
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'GGA'
070000030822     C                   EVAL      VABGGA=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* GVA
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'GVA'
070900030822     C                   EVAL      VABGVA=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* TC1
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'TC1'
071800030822     C                   EVAL      VABTC1=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* TC2
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'TC2'
072700030822     C                   EVAL      VABTC2=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100030822     C* ...
073200030822     C                   ENDIF
073300030822     C                   ENDDO
073400020204     C*
073500010330     C                   ENDSR
073600010607     C*----------------------------------------------------*
073700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
073800010607     C*----------------------------------------------------*
073900010607     C     IMPVAB        BEGSR
074000010607     C*
074100010607     C                   SETOFF                                       3132
074200030515     C*
074300010607     C                   MOVEL     datcor        VABAAS
074400010607     C                   MOVE      datcor        VABMGS
074500010607     C*
074600030715     C**********
074700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
074800030715     C**********
074900030715     C                   IF        wGiro = *zeros
075000040726     C*
075100040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
075200040726     C                   EVAL      depvinDTA = vinDTA
075300040726     C*
075400040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
075500040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
075600050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
075700050321     C                                      %trim(intcol(5))+%trim(intcol(6))+
075800050321     C                                      %trim(intcol(7))+%trim(intcol(8))+
075900050321     C                                      %trim(intcol(9))+%trim(intcol(10))
076000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
076100031201     C     minu:maiu     XLATE     vindta        vindta
076200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
076300040324     C                   DOW       posDa <= %len(%trimr(vindta))
076400030715     C*
076500030715     C* Gestisco il 1° campo
076600030715     C                   IF        i = 1
076700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
076800030715     C                   EVAL      posDa = 1
076900030715     C* Eseguo lo scan x trovare la fine del primo campo
077000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077100030715     C* A questo punto "estraggo" il campo corrente
077200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077300030715     C                                                    (posA-posDa))
077400030715     C* X i campi successivi al 1°
077500030715     C                   ELSE
077600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
077700030715     C                   EVAL      posDa = posA + 1
077800030715     C* Eseguo lo scan x trovare la fine del campo corrente
077900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078000030715     C* Gestisco l'ultimo campo
078100030715     C                   IF        posA = *zeros
078200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
078300030715     C* A questo punto "estraggo" il campo corrente
078400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078500030715     C                                                    (posA-posDa))
078600030715     C                   LEAVE
078700030715     C                   ELSE
078800030715     C* A questo punto "estraggo" il campo corrente
078900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079000030715     C                                                    (posA-posDa))
079100030715     C                   ENDIF
079200030715     C                   ENDIF
079300030715     C* Incremento il contatore d campo
079400030715     C                   EVAL      i = i +1
079500030715     C                   ENDDO
079600030715     C*
079700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079800030715     C* delimitazione testo
079900030715     C                   EVAL      i = 1
080000030715     C                   DOW       i < %elem(SkSplitFLD)
080100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
080200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
080300030715     C                   EVAL      i = i + 1
080400030715     C                   ENDDO
080500030715     C                   EVAL      wGiro = 1
080600040726     C* Ripristino la 1° riga originale
080700040726     C                   EVAL      vinDTA = depvinDTA
080800030715     C                   ELSE
080900030715     C                   EVAL      wGiro = 2
081000031204     C**********
081100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
081200031204     C* d separatore campo
081300031204     C**********
081400040726     C                   EVAL      vindta = %trim(vindta)
081500040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
081600040726     C                   EVAL      vindta = %subst(vindta:2)
081700040726     C                   ENDDO
081800031204     C*
081900031204     C                   Z-ADD     *zeros        lunghInput        4 0
082000040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
082100040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
082200040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
082300040726     C                   EVAL      lunghInput = %len(%trim(vindta))
082400040726     C                   ENDDO
082500030715     C**********
082600030715     C* Effettuo lo split del campo dati d input
082700030715     C**********
082800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082900040324     C                   DOW       posDa <= %len(%trimr(vindta))
083000030715     C*
083100030715     C* Gestisco il 1° campo
083200030715     C                   IF        i = 1
083300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083400030715     C                   EVAL      posDa = 1
083500030715     C* Eseguo lo scan x trovare la fine del primo campo
083600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C* X i campi successivi al 1°
084100030715     C                   ELSE
084200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084300030715     C                   EVAL      posDa = posA + 1
084400031204     C* Verifico che nn vi sia il campo nn valorizzato
084500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
084600031204     C* Se campo nn valorizzato skippo al prossimo
084700031204     C                   EVAL      SkSplitCSV(i) = *blanks
084800031204     C                   EVAL      posA  = posA + 1
084900031204     C                   ELSE
085000030715     C* Eseguo lo scan x trovare la fine del campo corrente
085100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085200030715     C* Gestisco l'ultimo campo
085300030715     C                   IF        posA = *zeros
085400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
085500030715     C* A questo punto "estraggo" il campo corrente
085600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085700030715     C                                                    (posA-posDa))
085800030715     C                   LEAVE
085900030715     C                   ELSE
086000030715     C* A questo punto "estraggo" il campo corrente
086100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086200030715     C                                                    (posA-posDa))
086300030715     C                   ENDIF
086400031204     C                   ENDIF
086500030715     C                   ENDIF
086600030715     C* Incremento il contatore d campo
086700030715     C                   EVAL      i = i +1
086800030715     C                   ENDDO
086900030715     C*
087000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087100030715     C* delimitazione testo
087200030715     C                   EVAL      i = 1
087300030715     C                   DOW       i < %elem(SkSplitCSV)
087400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
087500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
087600030715     C                   EVAL      i = i + 1
087700030715     C                   ENDDO
087800030715     C*
087900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
088000030715     C                   EVAL      i = 1
088100030715     C                   DOW       i < %elem(SkSplitFLD)
088200030715     C*
088300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
088400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
088500030715     C                             SkSplitCSV(i) <> *blanks
088600030721     C*** ATB
088700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
088800030721     C                   EVAL      VABATB = SkSplitCSV(i)
088900030721     C                   ENDIF
089000030715     C*** RSD
089100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
089200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
089300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
089400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
089500030715     C* ==
089600030715     C                   ENDIF
089700030721     C*** RD2
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
089900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
090000030721     C                   ENDIF
090100050811     C*** VATNOTB (tipo record 'B')
090200050811     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOTB'
090300050811     C                   ADD       1             jVAT
090400050811     C                   EVAL      wDSVAT_NOT = SkSplitCSV(i)
090500050811     C                   EVAL      wDSVAT_TRC = 'B'
090600050811     C                   EVAL      SkVAT(jVAT) = wDSVAT
090700050722     C                   ENDIF
090800030715     C*** IND
090900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
091000030715     C                   EVAL      VABIND = SkSplitCSV(i)
091100030715     C                   ENDIF
091200030715     C*** LOD
091300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
091400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
091500030715     C                   ENDIF
091600030715     C*** PRD
091700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
091800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
091900030715     C                   ENDIF
092000030721     C*** NZD
092100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
092200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
092300030721     C                   ENDIF
092400030715     C*** RMA
092500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
092600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
092700030715     C                   ENDIF
092800030715     C*** NOT
092900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
093000050811     C                   MOVEL     *blanks       wNOTE            70
093100050811     C                   EVAL      wNOTE = SkSplitCSV(i)
093200050811     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
093300050811     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
093400030715     C                   ENDIF
093500030715     C*** NT2
093600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
093700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
093800030715     C                   ENDIF
093900030715     C*** VCA
094000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094200030715     C                   ENDIF
094300030715     C*** VAS
094400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
094500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
094600030715     C                   ENDIF
094700030715     C*** FFD
094800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
094900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
095000030715     C                   ENDIF
095100030715     C*** CBO
095200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
095300030715     C                   EVAL      VABCBO = SkSplitCSV(i)
095400030715     C                   ENDIF
095500030715     C
095600030721     C*** GC1
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
095800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** GC2
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
096200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** TSP
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** NAS
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** TIC
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
097400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** GCA
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
097800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** XCO
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** CTM
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
098600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** TCR
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** CTS
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
099400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** FTM
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
099800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** VAD
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** GMA
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
100600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** GGA
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** GVA
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
101400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** TC1
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
101800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** TC2
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** SCL
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
102600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** RMO
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** NMO
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
103400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*
103700030721     C* Reperisco quindi i campi numerici...
103800030721     C*** CAD
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
104000030721     C                   EVAL      PiStr=SkSplitCSV(i)
104100030721     C                   EXSR      CHKNUM
104200030721     C                   IF        PiInt=*on
104300030721     C                   Z-ADD     PiVal         Num5_0
104400030721     C                   MOVEL(p)  Num5_0        VABCAD
104500030721     C                   ELSE
104600030721     C                   SETON                                        32
104700030721     C                   EVAL      VABCAD = *zeros
104800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104900030721     C                             + ' ' + 'VABCAD'
105000030721     C                   ENDIF
105100030721     C                   ENDIF
105200030721     C*** RMN
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
105400030721     C                   EVAL      PiStr=SkSplitCSV(i)
105500030721     C                   EXSR      CHKNUM
105600030721     C                   IF        PiInt=*on
105700030721     C                   Z-ADD     PiVal         VABRMN
105800030721     C                   ELSE
105900030721     C                   SETON                                        32
106000030721     C                   EVAL      VABRMN = 1
106100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106200030721     C                             + ' ' + 'VABRMN'
106300030721     C                   ENDIF
106400030721     C                   ENDIF
106500030721     C*** NSP
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
106700030721     C                   EVAL      PiStr=SkSplitCSV(i)
106800030721     C                   EXSR      CHKNUM
106900040726     C                   IF        PiInt=*on
107000040726     C                   Z-ADD     PiVal         VABNSP
107100040802     C*                  IF        PiVal>9999999
107200040802     C*                  SETON                                        32
107300040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
107400040802     C*                            + ' ' + 'VABNSP'
107500040802     C*                  ENDIF
107600030721     C                   ELSE
107700030721     C                   SETON                                        32
107800030721     C                   EVAL      VABNSP = *zeros
107900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108000030721     C                             + ' ' + 'VABNSP'
108100030721     C                   ENDIF
108200030721     C                   ENDIF
108300030721     C*** NCL
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
108500030721     C                   EVAL      PiStr=SkSplitCSV(i)
108600030721     C                   EXSR      CHKNUM
108700030721     C                   IF        PiInt=*on
108800030721     C                   Z-ADD     PiVal         VABNCL
108900030721     C                   ELSE
109000030721     C                   SETON                                        32
109100030721     C                   EVAL      VABNCL = *zeros
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300030721     C                             + ' ' + 'VABNCL'
109400030721     C                   ENDIF
109500030721     C                   ENDIF
109600030721     C*** PKB
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
109800030721     C                   EVAL      PiStr=SkSplitCSV(i)
109900030721     C                   EXSR      CHKNUM
110000030721     C                   IF        PiNum=*on
110100050811     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a Kg
110200030721     C                   Z-ADD     PiVal         VABPKB
110300030721     C                   ELSE
110400030721     C                   SETON                                        32
110500030721     C                   EVAL      VABPKB = *zeros
110600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110700030721     C                             + ' ' + 'VABPKB'
110800030721     C                   ENDIF
110900030721     C                   ENDIF
111000050505     C*** CAS
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiNum=*on
111500030721     C                   Z-ADD     PiVal         VABCAS
111600050811     C                   EVAL      VABVCA = 'EUR'
111700030721     C                   ELSE
111800030721     C                   SETON                                        32
111900030721     C                   EVAL      VABCAS = *zeros
112000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112100030721     C                             + ' ' + 'VABCAS'
112200030721     C                   ENDIF
112300030721     C                   ENDIF
112400030721     C*** CCM
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiInt=*on      AND
112900030721     C                             PiVal<=9999999 AND
113000030721     C                             PiVal>*zeros
113100030721     C                   Z-ADD     PiVal         VABCCM
113200030721     C                   ELSE
113300030721     C                   SETON                                        32
113400030721     C                   EVAL      VABCCM = *zeros
113500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113600030721     C                             + ' ' + 'VABCCM'
113700030721     C                   ENDIF
113800030721     C                   ENDIF
113900030721     C*** LNP
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiInt=*on    AND
114400030721     C                             PiVal<=999   AND
114500030721     C                             PiVal>*zeros
114600030721     C                   Z-ADD     PiVal         VABLNP
114700030721     C                   ELSE
114800030721     C                   SETON                                        32
114900030721     C                   EVAL      VABLNP = *zeros
115000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115100030721     C                             + ' ' + 'VABLNP'
115200030721     C                   ENDIF
115300030721     C                   ENDIF
115400030721     C*** NRS
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiInt=*on AND
115900030721     C                             PiVal<=99
116000030721     C                   Z-ADD     PiVal         VABNRS
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400030721     C                             + ' ' + 'VABNRS'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** LNA
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiInt=*on    AND
117200030721     C                             PiVal<=999   AND
117300030721     C                             PiVal>*zeros
117400030721     C                   Z-ADD     PiVal         VABLNA
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      VABLNA = *zeros
117800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117900030721     C                             + ' ' + 'VABLNA'
118000030721     C                   ENDIF
118100030721     C                   ENDIF
118200030721     C*** CTR
118300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
118400030721     C                   EVAL      PiStr=SkSplitCSV(i)
118500030721     C                   EXSR      CHKNUM
118600030721     C                   IF        PiInt=*on  AND
118700030721     C                             PiVal<=999
118800030721     C                   Z-ADD     PiVal         VABCTR
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119200030721     C                             + ' ' + 'VABCTR'
119300030721     C                   ENDIF
119400030721     C                   ENDIF
119500030721     C*** IAS
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800050623     C     X'20':' '     XLATE     PiStr         PiStr
119900050623     C                   EVAL      PiStr = %trim(PiStr)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiNum=*on
120200030721     C                   Z-ADD     PiVal         VABIAS
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600030721     C                             + ' ' + 'VABIAS'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** VLB
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiNum=*on
121400030721     C                   Z-ADD     PiVal         VABVLB
121500030721     C                   ELSE
121600030721     C                   SETON                                        32
121700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121800030721     C                             + ' ' + 'VABVLB'
121900030721     C                   ENDIF
122000030721     C                   ENDIF
122100030721     C*** QFT
122200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
122300030721     C                   EVAL      PiStr=SkSplitCSV(i)
122400030721     C                   EXSR      CHKNUM
122500030721     C                   IF        PiNum=*on
122600030721     C                   Z-ADD     PiVal         VABQFT
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000030721     C                             + ' ' + 'VABQFT'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** NCD
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on      AND
123800030721     C                             PiVal<=9999999
123900030721     C                   Z-ADD     PiVal         VABNCD
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124300030721     C                             + ' ' + 'VABNCD'
124400030721     C                   ENDIF
124500030721     C                   ENDIF
124600030721     C*** NCA
124700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
124800030721     C                   EVAL      PiStr=SkSplitCSV(i)
124900030721     C                   EXSR      CHKNUM
125000030721     C                   IF        PiInt=*on      AND
125100030721     C                             PiVal<=9999999
125200030721     C                   Z-ADD     PiVal         VABNCA
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABNCA'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** ZNC
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiInt=*on AND
126400030721     C                             PiVal<=99
126500030721     C                   Z-ADD     PiVal         VABZNC
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABZNC'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** DCR
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiInt=*on       AND
127700030721     C                             PiVal<=99999999
127800030721     C                   Z-ADD     PiVal         VABDCR
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABDCR'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** HCR
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiInt=*on   AND
129000030721     C                             PiVal<=9999
129100030721     C                   Z-ADD     PiVal         VABHCR
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129500030721     C                             + ' ' + 'VABHCR'
129600030721     C                   ENDIF
129700030721     C                   ENDIF
129800030721     C*** VMD
129900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
130000030721     C                   EVAL      PiStr=SkSplitCSV(i)
130100030721     C                   EXSR      CHKNUM
130200030721     C                   IF        PiNum=*on
130300030721     C                   Z-ADD     PiVal         VABVMD
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABVMD'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** ANT
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on        AND
131500030721     C                             PiVal<=999999999
131600030721     C                   Z-ADD     PiVal         VABANT
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABANT'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** CMO
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on
132800030721     C                   Z-ADD     PiVal         Num5_0
132900030721     C                   MOVEL(p)  Num5_0        VABCMO
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      VABCMO = *zeros
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400030721     C                             + ' ' + 'VABCMO'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700030715     C*
133800030715     C                   ENDIF
133900030715     C                   EVAL      i = i + 1
134000030715     C                   ENDDO
134100040726     C*
134200050811     C* Considerazioni personalizzate x il traduttore corrente
134300050811     C* NSP => Stacco un numeratore da AZNUM
134400050811     C                   clear                   TRUL33DS
134500050811     C                   eval      I33OPE = *zeros
134600050811     C                   eval      I33CNU = 302
134700050811     C                   eval      I33NUM = 1
134800050811     C                   movel     TRUL33DS      KPJBU
134900050811     C                   call      'TRUL33R'
135000050811     C                   parm                    KPJBA
135100050811     C                   movel     KPJBU         TRUL33DS
135200050811     C                   if        O33ERR = *zeros
135300050811     C                   z-add     O33NRF        VABNSP
135400050811     C                   z-add     O33NRF        VATNSP
135500050811     C                   else
135600050811     C                   EVAL      vinmsg = %trimr(vinmsg)
135700050811     C                             + ' ' + 'VABNSP VATNSP'
135800050811     C                   endif
135900051121     C*
136000051121     C* Se nazione destinatario = 'ITA' o 'IT' o 'I' => forzo *blanks
136100051121     C                   IF        VABNZD = 'I'   or
136200051121     C                             VABNZD = 'IT'  or
136300051121     C                             VABNZD = 'ITA'
136400051121     C                   EVAL      VABNZD = *blanks
136500051121     C                   ENDIF
136600050811     C*
136700050811     C* Reperisco la provincia dal CAP e dalla località
136800050811     C                   IF        VABCAD <> *blanks AND
136900050811     C                             VABLOD <> *blanks AND
137000050811     C                             VABPRD  = *blanks
137100050811     C                   CLEAR                   TISI95DS
137200050811     C                   EVAL      I95TCN = '3'
137300050811     C                   Z-ADD     datcor        I95DAT
137400050811     C                   EVAL      I95CAP = VABCAD
137500050811     C                   EVAL      I95LOC = VABLOD
137600050811     C                   CALL      'TISI95R'
137700050811     C                   PARM                    TISI95DS
137800050811     C                   EVAL      VABPRD = O95PRV
137900050811     C                   ENDIF
138000040726     C*
138100040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
138200040726     C                   IF        VABNSP = *zeros
138300040726     C                   SETON                                        31
138400040726     C                   ENDIF
138500040726     C*
138600040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
138700040726     C                   IF        VABLNP = *zeros
138800040726     C                   SETON                                        31
138900040726     C                   ENDIF
139000050623     C*
139100050623     C* Se CCM nn valorizzato considero errore bloccante in traduzione
139200050623     C                   IF        VABCCM = *zeros
139300050623     C                   SETON                                        31
139400050623     C                   ENDIF
139500040726     C*
139600040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
139700040726     C* d valorizzarli sempre entrambi
139800040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
139900040726     C                   Z-ADD     VABNSP        VABRMN
140000040726     C                   ENDIF
140100040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
140200040726     C                   MOVEL     VABRMN        VABRMA
140300040726     C                   ENDIF
140400040726     C*
140500040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
140600040726     C                   IF        VABCAS > *zeros
140700040726     C                   IF        VABVCA = *blanks
140800040726     C                   EVAL      VABVCA = 'EUR'
140900040726     C                   ENDIF
141000040726     C                   IF        VABCBO = '1'
141100040726     C                   EVAL      VABCBO = '4'
141200040726     C                   ENDIF
141300040726     C                   IF        VABCBO = '2'
141400040726     C                   EVAL      VABCBO = '6'
141500040726     C                   ENDIF
141600040726     C                   ENDIF
141700020204     C*
141800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
141900020204     C                   EXSR      CHKIMPDIV
142000030715     C                   ENDIF
142100020204     C*
142200010607     C                   ENDSR
142300010607     C*----------------------------------------------------*
142400010601
142500020204
142600020204     C*----------------------------------------------------*
142700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
142800020204     C*----------------------------------------------------*
142900020204     C     CHKIMPDIV     BEGSR
143000020204     C*
143100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
143200020204     C                   Z-ADD     *zeros        wrkDec            9 9
143300020204     C*
143400020204     C* Come prima cosa effettuo considerazioni sulla divisa
143500020204     C                   IF        vabIAS > *zeros
143600020204     C                   IF        vabVAS <> 'EUR'
143700020204     C                   EVAL      vabVAS =  'ITL'
143800020204     C                   ENDIF
143900020204     C                   ENDIF
144000020204     C*
144100020204     C                   IF        vabCAS > *zeros
144200020204     C                   IF        vabVCA <> 'EUR'
144300020204     C                   EVAL      vabVCA =  'ITL'
144400020204     C                   ENDIF
144500020204     C                   ENDIF
144600020204     C*
144700020204     C                   IF        vabVMD > *zeros
144800020204     C                   IF        vabVAD <> 'EUR'
144900020204     C                   EVAL      vabVAD =  'ITL'
145000020204     C                   ENDIF
145100020204     C                   ENDIF
145200020204     C*
145300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
145400020204     C                   Z-ADD     vabIAS        wrkDec
145500020204     C                   IF        wrkDec > *zeros
145600020204     C                   IF        vabVAS = 'ITL'
145700020204     C                   EVAL      vabIAS = *zeros
145800020204     C                   ENDIF
145900020204     C                   ENDIF
146000020204     C*
146100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
146200020204     C                   Z-ADD     vabCAS        wrkDec
146300020204     C                   IF        wrkDec > *zeros
146400020204     C                   IF        vabVCA = 'ITL'
146500020204     C                   EVAL      vabCAS = *zeros
146600020204     C                   ENDIF
146700020204     C                   ENDIF
146800020204     C*
146900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
147000020204     C                   Z-ADD     vabVMD        wrkDec
147100020204     C                   IF        wrkDec > *zeros
147200020204     C                   IF        vabVAD = 'ITL'
147300020204     C                   EVAL      vabVMD = *zeros
147400020204     C                   ENDIF
147500020204     C                   ENDIF
147600020204     C*
147700020204     C                   ENDSR
147800020204     C***
147900020204
148000010330
148100010330
148200010330     C*----------------------------------------------------*
148300010330     C*  CONTROLLO NUMERICITA' CAMPI
148400010330     C*----------------------------------------------------*
148500010330     C     CHKNUM        BEGSR
148600010330     C*
148700010606     C                   IF        PiDecChr = *blanks
148800030715     C                   EVAL      PiDecChr = CharNUM
148900010606     C                   ENDIF
149000010606     C*
149100010606     C                   CALL(e)   'ISNUMERIC'
149200010330     C                   PARM                    PiStr            30
149300010606     C                   PARM                    PiDecChr          1
149400010330     C                   PARM      *ZEROS        PiVal            30 9
149500010330     C                   PARM      '0'           PiInt             1
149600010330     C                   PARM      '0'           PiNum             1
149700010330     C                   IF        %error
149800010606     C                   EVAL      PiNum=*off
149900010330     C                   ENDIF
150000010330     C*
150100010330     C                   ENDSR
150200010330     C***
150300010330
150400010601
150500010601
150600010601
150700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
150800010601     C     repfil        BEGSR
150900010601     C*
151000010601     C                   if        invfil = *zeros and
151100010601     C                             depfil > *zeros and
151200010629     C                             (vinflg = *blanks or
151300010629     C                              vinflg = *zeros)
151400010601     C
151500010601     C                   eval      invfil = depfil
151600010601     C                   endif
151700010601     C*
151800010601     C                   if        depfil <> invfil and
151900010601     C                             invfil > *zeros
152000010601     C                   eval      flgMulti = '1'
152100010601     C                   if        vinflg = *blanks
152200010601     C                   add       1             cntNonEl
152300010601     C                   endif
152400010601     C                   endif
152500010601     C*
152600010601     C                   if        vinflg = '2'
152700010601     C                   eval      flgStato = '2'
152800010601     C                   endif
152900010601     C*
153000010601     C                   ENDSR
153100010601     C***
153200010601
153300010601
153400010601
153500010330
153600010330
153700010330
153800990920      /TITLE Invio dei dati al punto operativo.
153900000613     C     invio         BEGSR
154000050722     C*
154100050722     C* 1° invio FIVAT
154200050722     C                   reset                   dscmz
154300050722     C                   move      vlrpoi        cmzdst
154400050722     C                   eval      cmzfld = 'FIVATWWR'
154500050722     C                   eval      cmzmbd = vlrhdl
154600050722     C                   eval      %subst(cmzmbd:1:1) = 'M'
154700050722     C***                if        prmfir = *blanks
154800050722     C                   eval      cmzfla = 'FIVAT00F'
154900050722     C                   eval      cmzmba = 'FIVAT00F'
155000050722     C***                else
155100050722     C***                eval      cmzfla = prmfir
155200050722     C***                eval      cmzmba = prmfir
155300050722     C***                endif
155400050722     C                   eval      cmznrr = *zeros
155500050722     C                   move      §ctrokvt      cmznrr
155600050722     C                   eval      cmzlba = vlrfl1
155700050722     C                   call(e)   'TIS711C'
155800050722     C                   parm                    dscmz
155900050722     C                   parm      *blanks       esito
156000050722     C                   if        %error
156100050722     C                             or cmzerr = '1'
156200050722     C                             or esito  = '1'
156300050722     C                   eval      wrkesito = '3'
156400050722     C                   else
156500050722     C*
156600050722     C* 2° invio FIVAB
156700050722     C                   reset                   dscmz
156800050722     C                   move      vlrpoi        cmzdst
156900050722     C                   eval      cmzfld = vlrfou
157000050722     C                   eval      cmzmbd = vlrhdl
157100050722     C                   eval      %subst(cmzmbd:1:1) = 'M'
157200050722     C***                if        prmfir = *blanks
157300050722     C                   eval      cmzfla = 'FIVAB00F'
157400050722     C                   eval      cmzmba = 'FIVAB00F'
157500050722     C***                else
157600050722     C***                eval      cmzfla = prmfir
157700050722     C***                eval      cmzmba = prmfir
157800050722     C***                endif
157900050722     C                   eval      cmznrr = *zeros
158000050722     C                   move      §ctrokvb      cmznrr
158100050722     C                   eval      cmzlba = vlrfl1
158200050722     C                   call(e)   'TIS711C'
158300050722     C                   parm                    dscmz
158400050722     C                   parm      *blanks       esito
158500050722     C                   if        %error
158600050722     C                             or cmzerr = '1'
158700050722     C                             or esito  = '1'
158800050722     C                   eval      wrkesito = '3'
158900050722     C                   endif
159000050722     C                   endif
159100990920     C*
159200000613     C                   ENDSR
159300990910
159400010601
159500010601
159600010601
159700010601
159800010601      /TITLE Invio dei dati al punto operativo.
159900010601     C     opeini        BEGSR
160000010601     C*
160100010601     C* Inizializzo flag e contatori operativi
160200010601     C                   movel     '0'           flgGiro           1
160300010601     C                   movel     '0'           flgMulti          1
160400010601     C                   movel     '1'           flgStato          1
160500010615     C                   movel     '0'           flgOk             1
160600010601     C                   z-add     *zeros        cntNonEl         10 0
160700010601     C                   z-add     *zeros        depfil            3 0
160800010601     C                   z-add     *zeros        invfil            3 0
160900010601     C*
161000010601     C                   ENDSR
161100010601     C***
161200010601
161300010601
161400050421
161500050421
161600070411
161700070411     C     *pssr         BEGSR
161800070411     C*
161900070411     C                   if        %open(tivin00r)
162000070411     C                   close     tivin00r
162100070411     C                   endif
162200070411     C                   if        %open(fivabwwr)
162300070411     C                   close     fivabwwr
162400070411     C                   endif
162500070411     C                   if        %open(fivatwwr)
162600070411     C                   close     fivatwwr
162700070411     C                   endif
162800070411     C*
162900070411     C* Effettuo la chiamata al CLLE preposto
163000070411     C                   call(e)   'TITVVTC'
163100070411     C                   parm                    parccm
163200070411     C                   parm                    parmbr
163300070411     C                   parm      '2'           paropz
163400070411     C*
163500070411     C                   eval      wrkesito = '2'
163600070411     C*
163700070411     C                   seton                                        LR
163800070411     C*
163900070411     C                   ENDSR     '*CANCL'
164000070411     C***
164100070411
164200050421
164300010601
164400010330
164500010330
164600000613     C     *inzsr        BEGSR
164700990910     C*
164800990910     C     *entry        plist
164900990920     C                   parm                    tivlrds
165000990921     C                   parm      wrkesito      esito
165100000724     C                   parm                    prmlit
165200000710     C                   parm                    prmfir
165300050505     C*
165400050505     C* CALCOLA LA DATA CORRENTE
165500050505     C                   time                    wn14             14 0
165600050505     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
165700050505     C                   z-add     wn8           g08dat
165800050505     C                   z-add     *zeros        g08inv
165900050505     C                   movel     '0'           g08err
166000050505     C                   call      'XSRDA8'
166100050505     C                   parm                    wlbda8
166200050505     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
166300000613     C*
166400000613     C                   ENDSR
166500000613     C***
166600990908
166700030715     Otitvxlp   E            riepilogo         2
166800990915     O                                              'Upload via Internet'
166900990915     O                                           +1 'Traduzione TIVIN00R -'
167000021025     O                                           +1 'FIVABWWR'
167100990915     O                                           +1 'Testate bolle'
167200990915     O          E            riepilogo   2
167300990915     O                       wrkdata
167400990915     O                       wrkora              +1
167500990915     O                       procname            +1
167600990915     O          E            riepilogo   2
167700990915     O                                              'Cliente..................:'
167800010605     O                       VABCCM        z     +1
167900990915     O          E            riepilogo   2
168000990920     O                                              'Riferimento Strategi.....:'
168100990920     O                       vlrhdl              +1
168200990915     O          E            riepilogo   2
168300990915     O                                              'Giusti...................:'
168400050722     O                       §CTROKVB      2   +  1
168500990915     O          E            riepilogo   2
168600990915     O                                              'Sbagliati e corretti.....:'
168700971022     O                       §CTRMO        2   +  1
168800990915     O          E            riepilogo   2
168900990915     O                                              'Sbagliati e scartati.....:'
169000971022     O                       §CTRNO        2   +  1
169100000613
169200030715     Otitvxlps  E            testdett          2
169300000613     O                                              'Upload via Internet'
169400000613     O                                           +1 'Traduzione TIVIN00R -'
169500021025     O                                           +1 'FIVABWWR'
169600000613     O                                           +1 'Testate bolle'
169700000616     O          E            testdett    3
169800000613     O                                           +2 'N° rec'
169900000613     O                                           +3 'Anteprima contenuto'
170000000616     O          E            rigadett    2
170100000613     O                       rrnum               +2
170200000621     O                       recko               +3
170300000616     O          E            findett     2
170400000613     O                       wrkdata
170500000613     O                       wrkora              +1
170600000613     O                       procname            +1
170700000616     O          E            findett     2
170800000613     O                                              'Cliente..................:'
170900010605     O                       VABCCM        z     +1
171000000616     O          E            findett     2
171100000613     O                                              'Riferimento Strategi.....:'
171200000613     O                       vlrhdl              +1
171300000616     O          E            findett     2
171400000613     O                                              'Giusti...................:'
171500050722     O                       §CTROKVB      2   +  1
171600000616     O          E            findett     2
171700000613     O                                              'Sbagliati e corretti.....:'
171800000613     O                       §CTRMO        2   +  1
171900000616     O          E            findett     2
172000000613     O                                              'Sbagliati e scartati.....:'
172100000613     O                       §CTRNO        2   +  1
172200000613** CMD - COMANDI CL
172300030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
172400030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
172500030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
172600010606
172700000613
172800050505** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
172900051121VABRMA;VABRMO;VABRSD;VABRD2;FILLER1;VABIND;FILLER2;VABNZD;
173000051121VABCAD;VABLOD;VATNOTB;VABCAS;VABNOT;VABPKB;VABRMN;VABNCL
173100051121                                               
173200051121
173300051121
173400051121
173500051121
173600051121
173700051121
173800051121
