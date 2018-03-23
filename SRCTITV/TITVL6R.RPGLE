000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800050330     Ftitvl6p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050330     Ftitvl6ps  O    f  198        PRINTER usropn
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
004000040119     D parccm          s              8    INZ(*blanks)
004100040119     D parmbr          s             10    INZ(*blanks)
004200040119     D paropz          s              1    INZ(*blanks)
004300040119     D chkcall         s              1    INZ(*blanks)
004400040706     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004500030715     D*------------------
004600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004700030715     D*------------------
004800030715     D SkSplitFLD      S             10    DIM(100)
004900030715     D SkSplitCSV      S            256    DIM(100)
005000030715     D CharCSV         S              1
005100030715     D CharTXT         S              1
005200030715     D CharNUM         S              1
005300030715     D posDa           S              3  0 INZ(*zeros)
005400030715     D posA            S              3  0 INZ(*zeros)
005500030715     D i               s              3  0 INZ(1)
005600030715     D wGiro           s              1  0 INZ(*zeros)
005700030822     D*------------------
005800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005900030822     D*------------------
006000030822     D posDaDft        S              3  0 INZ(*zeros)
006100030822     D posADft         S              3  0 INZ(*zeros)
006200030822     D j               s              3  0 INZ(1)
006300030822     D wGiroDft        s              1  0 INZ(*zeros)
006400010330     D*------------------
006500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600010330     D*------------------
006700010330     D WLBDA8          DS                  INZ
006800010330     D  G08DAT                 1      8  0
006900010330     D  G08INV                 9     16  0
007000010330     D  G08ERR                17     17
007100010330     D  G08TGI                18     22  0
007200031201     D*------------------
007300031201     D* Costanti
007400031201     D*------------------
007500031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007600031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007700010330
007800010330
007900010330
008000990915     C                   time                    wrkdata
008100990915     C                   time                    wrkora
008200000913     C                   reset                   rrnum
008300990921     C                   reset                   esito
008400990921     C                   reset                   wrkesito
008500010601     C*
008600010601     C                   exsr      opeini
008700010605     C                   exsr      rwvab
008800010601     C*
008900010601     C                   seton                                        lr
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
013000050330     C                   if        not %open(titvl6ps)
013100050330     C                   open      titvl6ps
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
014300010601     C*--------------------------------------------------------
014400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014500010601     C*--------------------------------------------------------
014600010601     C     ENDELA        BEGSR
014700990915     C*
014800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014900040119     C*
015000040119     C* Effettuo la chiamata al CLLE preposto
015100040119     C                   call(e)   'TITVVTC'
015200040119     C                   parm                    parccm
015300040119     C                   parm                    parmbr
015400040119     C                   parm      '2'           paropz
015500050201     C*
015600050201     C* Effettuo lancio TISI95 solo x chiusura
015700050201     C                   CLEAR                   TISI95DS
015800050201     C                   EVAL      I95TLA = 'C'
015900050201     C                   CALL      'TISI95R'
016000050201     C                   PARM                    TISI95DS
016100000613     C*
016200050330     C                   if        %open(titvl6ps)
016300000616     C                   except    findett
016400050330     C                   close     titvl6ps
016500000613     C                   endif
016600000616     C*
016700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016800011113     C                   Z-ADD     150           LENGH            15 5
016900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017000000616     C                   PARM                    CMD(3)
017100000616     C                   PARM                    LENGH
017200000616     C*
017300010601     C                   ENDSR
017400010601     C***
017500010601
017600010601
017700010601
017800000613
017900000613     C*--------------------------------------------------------
018000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018100000613     C*--------------------------------------------------------
018200000621     C     STPR          BEGSR
018300000613     C*
018400050330     C                   if        not %open(titvl6p)
018500050330     C   50              open      titvl6p
018600990915     C                   endif
018700990915     C*
018800030715     C   50              except    riepilogo
018900990915     C*
019000050330     C                   if        %open(titvl6p)
019100050330     C   50              close     titvl6p
019200990914     C                   endif
019300000613     C*
019400000613     C                   ENDSR
019500000613     C***
019600000613
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
021000040119     C*
021100040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021200040119     C                   exsr      prevat
021300040119     C*
021400040119     C                   if        chkcall = '0'
021500040119     C*
021600040119     C                   if        not %open(fivatwwr)
021700040119     C                   open      fivatwwr
021800040119     C                   endif
021900010330     C*
022000040119     C                   clear                   §CTROKVB          7 0
022100040119     C                   clear                   §CTROKVT          7 0
022200040119     C                   clear                   §CTRMO            7 0
022300040119     C                   clear                   §CTRNO            7 0
022400050330     C*
022500050330     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
022600050330     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
022700050330     C                   exsr      inzvar
022800050330     C                   exsr      impvab
022900050330     C*
023000050330     C                   do        *HIVAL
023100010330     C*
023200040706     C                   read      tivin00r                               70
023300010618     C*
023400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023500010618     C                   if        vinflg = '1'
023600010618     C                   eval      flgOk = '1'
023700010618     C                   endif
023800050330     C*
023900050330     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024000050330     C                   z-add     *zeros        wLenVINDTA        4 0
024100050330     C                   z-add     *zeros        wFlgVINDTA        1 0
024200050330     C                   dow       wLenVINDTA < %len(%trim(vindta))
024300050330     C                   eval      wLenVINDTA = wLenVINDTA + 1
024400050330     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024500050330     C                   z-add     1             wFlgVINDTA
024600050330     C                   leave
024700050330     C                   endif
024800050330     C                   enddo
024900010618     C*
025000050330     C                   if        vindta > *blanks AND
025100050330     C                             wFlgVINDTA = 1
025200010330     C                   add       1             rrnum
025300010330     C*
025400010601     C                   if        *in70 = *off and
025500010330     C                             (vinflg = *blanks
025600010330     C                              or vinflg = '0'
025700010330     C                              or vinflg = '2')
025800010330     C*
025900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026000010711     C                   if        vinflg = *blanks or vinflg = '0'
026100010711     C                   clear                   vinmsg
026200010711     C                   endif
026300010601     C*
026400010330     C                   exsr      inzvar
026500010605     C                   exsr      impvab
026600010601     C*
026700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026800010605     C                   eval      depfil = VABLNP
026900010601     C                   exsr      repfil
027000010601     C                   if        depfil = invfil
027100021025     C                   if        vlrpoi = 999
027200030822     C                   move(P)   invfil        VABFGS
027300040119     C                   move(P)   invfil        VATFGS
027400021025     C                   else
027500030822     C                   move(P)   vlrpoi        VABFGS
027600040119     C                   move(P)   vlrpoi        VATFGS
027700021025     C                   endif
027800010601     C*
027900010601     C                   exsr      PREELA
028000010601     C*
028100010604     C* Ebbene...
028200010604     C*
028300040119     C  N31              add       1             §CTROKVB
028400040119     C   32              add       1             §CTRMO
028500040119     C   31              add       1             §CTRNO
028600030822     C                   if        wGiro = 2
028700040119     C  N31              exsr      wrivab
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
037100040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
037200050330     C                   exsr      wrivat
037300040119     C*
037400040119     C                   ENDSR
037500040119
037600040119
037700040119
037800040119     C*----------------------------------------------------*
037900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038000040119     C*----------------------------------------------------*
038100040706     C     WRIVAT        BEGSR
038200040706     C*
038300040706     C* Attribuisco lo stesso numero numero spedizione del file testate
038400040706     C                   EVAL      VATNSP = VABNSP
038500040119     C*
038600050330     C                   MOVEL     VABRMA        VATNOT
038700050330     C                   write     FIVAT000
038800040119     C*
038900040119     C                   ENDSR
039000040119
039100040119
039200040119
039300040119     C*----------------------------------------------------*
039400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
039500040119     C*----------------------------------------------------*
039600040119     C     PREVAT        BEGSR
039700040119     C*
039800040119     C* Compongo il nome del membro da dare al FIVATWWR
039900040119     C                   eval      parmbr = vlrhdl
040000040119     C                   movel     'M'           parmbr
040100050330     C                   eval      parccm = %subst(vlrKSC:2:7)
040200040119     C                   eval      paropz = '1'
040300040119     C* Effettuo la chiamata al CLLE preposto
040400040119     C                   call(e)   'TITVVTC'
040500040119     C                   parm                    parccm
040600040119     C                   parm                    parmbr
040700040119     C                   parm                    paropz
040800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040900040119     C                   if        %error
041000040119     C                   movel     '1'           chkcall
041100040119     C                   else
041200040119     C                   movel     '0'           chkcall
041300040119     C                   endif
041400040119     C*
041500040119     C                   ENDSR
041600010601
041700010601
041800010601
041900010330     C*----------------------------------------------------*
042000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
042100010330     C*----------------------------------------------------*
042200040119     C     INZVAR        BEGSR
042300010330     C*
042400030822     C* Inizializzo variabili di wrk
042500020204     C                   Z-ADD     *zeros        Num5_0            5 0
042600030822     C*
042700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
042800030822     C                   CLEAR                   FIVAB000
042900040119     C                   CLEAR                   FIVAT000
043000030822     C                   CLEAR                   SkSplitCSV
043100030822     C                   EVAL      i = 1
043200030822     C                   EVAL      posDa = *zeros
043300030822     C                   EVAL      posA  = *zeros
043400030822     C*
043500030822     C* Reimposto i valori di default
043600030822     C                   EXSR      DEFCAM
043700030822     C*
043800010330     C                   ENDSR
043900010330     C*----------------------------------------------------*
044000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
044100010330     C*----------------------------------------------------*
044200010330     C     DEFCAM        BEGSR
044300040119     C*
044400040119     C* Imposto i valori d default a programma dei campi fondamentali
044500050330     C                   EVAL      VABCCM = 0470779
044600050330     C                   EVAL      VATCCM = 0470779
044700050330     C                   EVAL      VABLNP = 047
044800050330     C                   EVAL      VATLNP = 047
044900040706     C                   EVAL      VABCTR = 000
045000040119     C                   EVAL      VABCTM = '7Q'
045100040119     C                   EVAL      VABCBO = '1'
045200040119     C                   EVAL      VATTRC = 'E'
045300030822     C*
045400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
045500030715     C* e delimitatore testo.
045600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
045700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
045800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
045900050330     C*
046000050330     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
046100050330     C                   IF        %subst(vlrppt:1:1) = 'S'
046200050330     C                   SETON                                        50
046300050330     C                   ELSE
046400050330     C                   SETOFF                                       50
046500050330     C                   ENDIF
046600030822     C*
046700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
046800030822     C                   EVAL      posDaDft = 1
046900030822     C                   EVAL      posADft  = 0
047000030822     C                   EVAL      wGiroDft = 0
047100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
047200030822     C                             posDaDft > 0
047300030822     C*
047400030822     C* Gestisco il 1° giro
047500030822     C                   IF        wGiroDft = 0
047600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
047700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
047800030822     C* Incremento il contatore dei "giri"
047900030822     C                   EVAL      wGiroDft = 1
048000030822     C                   ELSE
048100030822     C                   EVAL      posDaDft = posADft
048200030822     C                   ENDIF
048300030822     C* Eseguo lo scan x trovare la fine del campo corrente
048400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
048500030822     C*
048600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
048700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
048800030822     C                   IF        posDaDft > 0 AND
048900030822     C                             posADft  > 0
049000030822     C* NCL
049100030822     C                   IF        %subst(
049200030822     C                             %subst(vlrppt:posDaDft+1:
049300030822     C                             posADft-posDaDft-1):1:3)
049400030822     C                             = 'NCL'
049500030822     C                   EVAL      PiStr=%trim(%subst(
049600030822     C                             %subst(vlrppt:posDaDft+1:
049700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049800030822     C                   EXSR      CHKNUM
049900030822     C                   IF        PiInt=*on
050000030822     C                   Z-ADD     PiVal         VABNCL
050100030822     C                   ENDIF
050200030822     C                   ENDIF
050300030822     C* CCM
050400030822     C                   IF        %subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):1:3)
050700030822     C                             = 'CCM'
050800030822     C                   EVAL      PiStr=%trim(%subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051100030822     C                   EXSR      CHKNUM
051200030822     C                   IF        PiInt=*on
051300030822     C                   Z-ADD     PiVal         VABCCM
051400040119     C                   Z-ADD     PiVal         VATCCM
051500030822     C                   ENDIF
051600030822     C                   ENDIF
051700030822     C* LNP
051800030822     C                   IF        %subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):1:3)
052100030822     C                             = 'LNP'
052200030822     C                   EVAL      PiStr=%trim(%subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052500030822     C                   EXSR      CHKNUM
052600030822     C                   IF        PiInt=*on
052700030822     C                   Z-ADD     PiVal         VABLNP
052800040119     C                   Z-ADD     PiVal         VATLNP
052900030822     C                   ENDIF
053000030822     C                   ENDIF
053100030822     C* NRS
053200030822     C                   IF        %subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):1:3)
053500030822     C                             = 'NRS'
053600030822     C                   EVAL      PiStr=%trim(%subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053900030822     C                   EXSR      CHKNUM
054000030822     C                   IF        PiInt=*on
054100030822     C                   Z-ADD     PiVal         VABNRS
054200030822     C                   ENDIF
054300030822     C                   ENDIF
054400030822     C* CTR
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800030822     C                             = 'CTR'
054900030822     C                   EVAL      PiStr=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   EXSR      CHKNUM
055300030822     C                   IF        PiInt=*on
055400030822     C                   Z-ADD     PiVal         VABCTR
055500030822     C                   ENDIF
055600030822     C                   ENDIF
055700030822     C* PKB
055800030822     C                   IF        %subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):1:3)
056100030822     C                             = 'PKB'
056200030822     C                   EVAL      PiStr=%trim(%subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500030822     C                   EXSR      CHKNUM
056600030822     C                   IF        PiNum=*on
056700050330     C                   Z-ADD     PiVal         VABPKB
056800030822     C                   ENDIF
056900030822     C                   ENDIF
057000030822     C* VLB
057100030822     C                   IF        %subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):1:3)
057400030822     C                             = 'VLB'
057500030822     C                   EVAL      PiStr=%trim(%subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800030822     C                   EXSR      CHKNUM
057900030822     C                   IF        PiNum=*on
058000030822     C                   Z-ADD     PiVal         VABVLB
058100030822     C                   ENDIF
058200030822     C                   ENDIF
058300030822     C* QFT
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'QFT'
058800030822     C                   EVAL      PiStr=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   EXSR      CHKNUM
059200030822     C                   IF        PiNum=*on
059300030822     C                   Z-ADD     PiVal         VABQFT
059400030822     C                   ENDIF
059500030822     C                   ENDIF
059600030822     C* CBO
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'CBO'
060100030822     C                   EVAL      VABCBO=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   ENDIF
060500030822     C* TSP
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900030822     C                             = 'TSP'
061000030822     C                   EVAL      VABTSP=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   ENDIF
061400030822     C* VAS
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'VAS'
061900030822     C                   EVAL      VABVAS=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   ENDIF
062300030822     C* VCA
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'VCA'
062800030822     C                   EVAL      VABVCA=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   ENDIF
063200030822     C* GCA
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'GCA'
063700030822     C                   EVAL      VABGCA=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   ENDIF
064100030822     C* CTM
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'CTM'
064600030822     C                   EVAL      VABCTM=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   ENDIF
065000030822     C* FFD
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'FFD'
065500030822     C                   EVAL      VABFFD=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* VAD
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'VAD'
066400030822     C                   EVAL      VABVAD=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800030822     C* GMA
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'GMA'
067300030822     C                   EVAL      VABGMA=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   ENDIF
067700030822     C* GGA
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'GGA'
068200030822     C                   EVAL      VABGGA=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   ENDIF
068600030822     C* GVA
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'GVA'
069100030822     C                   EVAL      VABGVA=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   ENDIF
069500030822     C* TC1
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'TC1'
070000030822     C                   EVAL      VABTC1=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* TC2
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'TC2'
070900030822     C                   EVAL      VABTC2=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* ...
071400030822     C                   ENDIF
071500030822     C                   ENDDO
071600020204     C*
071700010330     C                   ENDSR
071800010607     C*----------------------------------------------------*
071900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
072000010607     C*----------------------------------------------------*
072100010607     C     IMPVAB        BEGSR
072200010607     C*
072300010607     C                   SETOFF                                       3132
072400030515     C*
072500010607     C                   MOVEL     datcor        VABAAS
072600040119     C                   MOVEL     datcor        VATAAS
072700010607     C                   MOVE      datcor        VABMGS
072800010607     C*
072900030715     C**********
073000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073100030715     C**********
073200030715     C                   IF        wGiro = *zeros
073300040706     C*
073400040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
073500040706     C                   EVAL      depvinDTA = vinDTA
073600040113     C*
073700040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
073800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
073900040113     C                                      %trim(intcol(3))
074000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
074100031201     C     minu:maiu     XLATE     vindta        vindta
074200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074300030715     C                   DOW       posDa <= %len(%trim(vindta))
074400030715     C*
074500030715     C* Gestisco il 1° campo
074600030715     C                   IF        i = 1
074700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
074800030715     C                   EVAL      posDa = 1
074900030715     C* Eseguo lo scan x trovare la fine del primo campo
075000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075100030715     C* A questo punto "estraggo" il campo corrente
075200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075300030715     C                                                    (posA-posDa))
075400030715     C* X i campi successivi al 1°
075500030715     C                   ELSE
075600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
075700030715     C                   EVAL      posDa = posA + 1
075800030715     C* Eseguo lo scan x trovare la fine del campo corrente
075900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076000030715     C* Gestisco l'ultimo campo
076100030715     C                   IF        posA = *zeros
076200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
076300030715     C* A questo punto "estraggo" il campo corrente
076400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076500030715     C                                                    (posA-posDa))
076600030715     C                   LEAVE
076700030715     C                   ELSE
076800030715     C* A questo punto "estraggo" il campo corrente
076900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077000030715     C                                                    (posA-posDa))
077100030715     C                   ENDIF
077200030715     C                   ENDIF
077300030715     C* Incremento il contatore d campo
077400030715     C                   EVAL      i = i +1
077500030715     C                   ENDDO
077600030715     C*
077700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
077800030715     C* delimitazione testo
077900030715     C                   EVAL      i = 1
078000030715     C                   DOW       i < %elem(SkSplitFLD)
078100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
078200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
078300030715     C                   EVAL      i = i + 1
078400030715     C                   ENDDO
078500030715     C                   EVAL      wGiro = 1
078600040706     C* Ripristino la 1° riga originale
078700040706     C                   EVAL      vinDTA = depvinDTA
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
087200050330     C*******            EVAL      VABRSD = SkSplitCSV(i)
087300050330      * nel primo byte della ragione sociale del destinatario c'è una 'O' da togliere
087400050330     c                   clear                   wrsd             36
087500050330     C                   EVAL      wrsd = SkSplitCSV(i)
087600050330     C                   EVAL      VABRSD = %trim(%subst(wrsd:2:35))
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
100500040119     C*** CBO
100600040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100700040706     C                   EVAL      VABCBO = SkSplitCSV(i)
100800040119     C                   ENDIF
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
102400050330     C*** RMN/RMA
102500040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
102600050330     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
102700040706     C                   EVAL      PiStr=SkSplitCSV(i)
102800040706     C                   EXSR      CHKNUM
102900040706     C                   IF        PiInt=*on
103000040706     C                   Z-ADD     PiVal         VABRMN
103100040706     C                   ELSE
103200040706     C                   SETON                                        32
103300040706     C                   EVAL      VABRMN = 1
103400040706     C                   EVAL      vinmsg = %trimr(vinmsg)
103500040706     C                             + ' ' + 'VABRMN'
103600040706     C                   ENDIF
103700040706     C                   ENDIF
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
105200030721     C*** NCL
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
105400030721     C                   EVAL      PiStr=SkSplitCSV(i)
105500030721     C                   EXSR      CHKNUM
105600030721     C                   IF        PiInt=*on
105700030721     C                   Z-ADD     PiVal         VABNCL
105800030721     C                   ELSE
105900030721     C                   SETON                                        32
106000030721     C                   EVAL      VABNCL = *zeros
106100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106200040706     C                             + ' ' + 'VABNCL'
106300030721     C                   ENDIF
106400030721     C                   ENDIF
106500030721     C*** PKB
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
106700030721     C                   EVAL      PiStr=SkSplitCSV(i)
106800030721     C                   EXSR      CHKNUM
106900030721     C                   IF        PiNum=*on
107000050330     C                   Z-ADD(h)  PiVal         VABPKB
107100030721     C                   ELSE
107200030721     C                   SETON                                        32
107300030721     C                   EVAL      VABPKB = *zeros
107400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107500030721     C                             + ' ' + 'VABPKB'
107600030721     C                   ENDIF
107700030721     C                   ENDIF
107800030721     C*** CAS
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
108000030721     C                   EVAL      PiStr=SkSplitCSV(i)
108100030721     C                   EXSR      CHKNUM
108200030721     C                   IF        PiNum=*on
108300030721     C                   Z-ADD     PiVal         VABCAS
108400030721     C                   ELSE
108500030721     C                   SETON                                        32
108600030721     C                   EVAL      VABCAS = *zeros
108700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108800030721     C                             + ' ' + 'VABCAS'
108900030721     C                   ENDIF
109000030721     C                   ENDIF
109100030721     C*** CCM
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
109300030721     C                   EVAL      PiStr=SkSplitCSV(i)
109400030721     C                   EXSR      CHKNUM
109500030721     C                   IF        PiInt=*on      AND
109600030721     C                             PiVal<=9999999 AND
109700030721     C                             PiVal>*zeros
109800030721     C                   Z-ADD     PiVal         VABCCM
109900040119     C                   Z-ADD     PiVal         VATCCM
110000030721     C                   ELSE
110100030721     C                   SETON                                        32
110200030721     C                   EVAL      VABCCM = *zeros
110300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110400040119     C                             + ' ' + 'VABCCM VATCCM'
110500030721     C                   ENDIF
110600030721     C                   ENDIF
110700030721     C*** LNP
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
110900030721     C                   EVAL      PiStr=SkSplitCSV(i)
111000030721     C                   EXSR      CHKNUM
111100030721     C                   IF        PiInt=*on    AND
111200030721     C                             PiVal<=999   AND
111300030721     C                             PiVal>*zeros
111400030721     C                   Z-ADD     PiVal         VABLNP
111500040119     C                   Z-ADD     PiVal         VATLNP
111600030721     C                   ELSE
111700030721     C                   SETON                                        32
111800030721     C                   EVAL      VABLNP = *zeros
111900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112000040119     C                             + ' ' + 'VABLNP VATLNP'
112100030721     C                   ENDIF
112200030721     C                   ENDIF
112300030721     C*** NRS
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
112500030721     C                   EVAL      PiStr=SkSplitCSV(i)
112600030721     C                   EXSR      CHKNUM
112700030721     C                   IF        PiInt=*on AND
112800030721     C                             PiVal<=99
112900030721     C                   Z-ADD     PiVal         VABNRS
113000030721     C                   ELSE
113100030721     C                   SETON                                        32
113200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113300030721     C                             + ' ' + 'VABNRS'
113400030721     C                   ENDIF
113500030721     C                   ENDIF
113600030721     C*** LNA
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
113800030721     C                   EVAL      PiStr=SkSplitCSV(i)
113900030721     C                   EXSR      CHKNUM
114000030721     C                   IF        PiInt=*on    AND
114100030721     C                             PiVal<=999   AND
114200030721     C                             PiVal>*zeros
114300030721     C                   Z-ADD     PiVal         VABLNA
114400030721     C                   ELSE
114500030721     C                   SETON                                        32
114600030721     C                   EVAL      VABLNA = *zeros
114700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114800030721     C                             + ' ' + 'VABLNA'
114900030721     C                   ENDIF
115000030721     C                   ENDIF
115100030721     C*** CTR
115200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiInt=*on  AND
115600030721     C                             PiVal<=999
115700030721     C                   Z-ADD     PiVal         VABCTR
115800030721     C                   ELSE
115900030721     C                   SETON                                        32
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABCTR'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** IAS
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiNum=*on
116900030721     C                   Z-ADD     PiVal         VABIAS
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300030721     C                             + ' ' + 'VABIAS'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** VLB
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiNum=*on
118100030721     C                   Z-ADD     PiVal         VABVLB
118200030721     C                   ELSE
118300030721     C                   SETON                                        32
118400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118500030721     C                             + ' ' + 'VABVLB'
118600030721     C                   ENDIF
118700030721     C                   ENDIF
118800030721     C*** QFT
118900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
119000030721     C                   EVAL      PiStr=SkSplitCSV(i)
119100030721     C                   EXSR      CHKNUM
119200030721     C                   IF        PiNum=*on
119300030721     C                   Z-ADD     PiVal         VABQFT
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700030721     C                             + ' ' + 'VABQFT'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030721     C*** NCD
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiInt=*on      AND
120500030721     C                             PiVal<=9999999
120600030721     C                   Z-ADD     PiVal         VABNCD
120700030721     C                   ELSE
120800030721     C                   SETON                                        32
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABNCD'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** NCA
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on      AND
121800030721     C                             PiVal<=9999999
121900030721     C                   Z-ADD     PiVal         VABNCA
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABNCA'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** ZNC
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on AND
123100030721     C                             PiVal<=99
123200030721     C                   Z-ADD     PiVal         VABZNC
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABZNC'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** DCR
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on       AND
124400030721     C                             PiVal<=99999999
124500030721     C                   Z-ADD     PiVal         VABDCR
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABDCR'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** HCR
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiInt=*on   AND
125700030721     C                             PiVal<=9999
125800030721     C                   Z-ADD     PiVal         VABHCR
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126200030721     C                             + ' ' + 'VABHCR'
126300030721     C                   ENDIF
126400030721     C                   ENDIF
126500030721     C*** VMD
126600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
126700030721     C                   EVAL      PiStr=SkSplitCSV(i)
126800030721     C                   EXSR      CHKNUM
126900030721     C                   IF        PiNum=*on
127000030721     C                   Z-ADD     PiVal         VABVMD
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABVMD'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700030721     C*** ANT
127800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiInt=*on        AND
128200030721     C                             PiVal<=999999999
128300030721     C                   Z-ADD     PiVal         VABANT
128400030721     C                   ELSE
128500030721     C                   SETON                                        32
128600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128700030721     C                             + ' ' + 'VABANT'
128800030721     C                   ENDIF
128900030721     C                   ENDIF
129000030721     C*** CMO
129100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
129200030721     C                   EVAL      PiStr=SkSplitCSV(i)
129300030721     C                   EXSR      CHKNUM
129400030721     C                   IF        PiInt=*on
129500030721     C                   Z-ADD     PiVal         Num5_0
129600030721     C                   MOVEL(p)  Num5_0        VABCMO
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      VABCMO = *zeros
130000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130100030721     C                             + ' ' + 'VABCMO'
130200030721     C                   ENDIF
130300030721     C                   ENDIF
130400040706     C*
130500040706     C* Gestisco i campi "personalizzati"
130600050330     C*** CAPLOC
130700050330     C                   IF        %trim(SkSplitFLD(i))='CAPLOC'
130800050330     C                   EVAL      PiStr = %trim(%subst(sksplitcsv(i):1:5))
130900040706     C                   EXSR      CHKNUM
131000040706     C                   IF        PiInt=*on
131100050330     C                   Z-ADD     PiVal         Num5_0
131200050330     C                   MOVEL(p)  Num5_0        VABCAD
131300050330     C                   ELSE
131400050330     C                   SETON                                        32
131500050330     C                   EVAL      VABCAD = *zeros
131600050330     C                   EVAL      vinmsg = %trimr(vinmsg)
131700050330     C                             + ' ' + 'VABCAD'
131800050330     C                   ENDIF
131900050330     C                   EVAL      VABLOD = %trim(%subst(SkSplitCSV(i):6:posa))
132000040706     C                   ENDIF
132100030715     C*
132200030715     C                   ENDIF
132300030715     C                   EVAL      i = i + 1
132400030715     C                   ENDDO
132500040706     C*
132600040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
132700040706     C* Reperisco la provincia dal CAP e dalla località
132800040706     C                   IF        VABCAD <> *blanks
132900040706     C                   CLEAR                   TISI95DS
133000040706     C                   EVAL      I95TCN = '3'
133100040706     C                   Z-ADD     datcor        I95DAT
133200040706     C                   EVAL      I95CAP = VABCAD
133300040706     C                   EVAL      I95LOC = VABLOD
133400040706     C                   CALL      'TISI95R'
133500040706     C                   PARM                    TISI95DS
133600040706     C                   EVAL      VABPRD = O95PRV
133700040706     C                   ENDIF
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
158000070411
158100050421
158200010601
158300010330
158400010330
158500000613     C     *inzsr        BEGSR
158600990910     C*
158700990910     C     *entry        plist
158800990920     C                   parm                    tivlrds
158900990921     C                   parm      wrkesito      esito
159000000724     C                   parm                    prmlit
159100000710     C                   parm                    prmfir
159200010330     C*
159300010330     C* CALCOLA LA DATA CORRENTE
159400010330     C                   time                    wn14             14 0
159500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
159600010330     C                   z-add     wn8           g08dat
159700010330     C                   z-add     *zeros        g08inv
159800010330     C                   movel     '0'           g08err
159900010330     C                   call      'XSRDA8'
160000010330     C                   parm                    wlbda8
160100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
160200000613     C*
160300000613     C                   ENDSR
160400000613     C***
160500990908
160600050330     Otitvl6p   E            riepilogo         2
160700990915     O                                              'Upload via Internet'
160800990915     O                                           +1 'Traduzione TIVIN00R -'
160900021025     O                                           +1 'FIVABWWR'
161000990915     O                                           +1 'Testate bolle'
161100990915     O          E            riepilogo   2
161200990915     O                       wrkdata
161300990915     O                       wrkora              +1
161400990915     O                       procname            +1
161500990915     O          E            riepilogo   2
161600990915     O                                              'Cliente..................:'
161700010605     O                       VABCCM        z     +1
161800990915     O          E            riepilogo   2
161900990920     O                                              'Riferimento Strategi.....:'
162000990920     O                       vlrhdl              +1
162100990915     O          E            riepilogo   2
162200990915     O                                              'Giusti...................:'
162300040119     O                       §CTROKVB      2   +  1
162400990915     O          E            riepilogo   2
162500990915     O                                              'Sbagliati e corretti.....:'
162600971022     O                       §CTRMO        2   +  1
162700990915     O          E            riepilogo   2
162800990915     O                                              'Sbagliati e scartati.....:'
162900971022     O                       §CTRNO        2   +  1
163000000613
163100050330     Otitvl6ps  E            testdett          2
163200000613     O                                              'Upload via Internet'
163300000613     O                                           +1 'Traduzione TIVIN00R -'
163400021025     O                                           +1 'FIVABWWR'
163500000613     O                                           +1 'Testate bolle'
163600000616     O          E            testdett    3
163700000613     O                                           +2 'N° rec'
163800000613     O                                           +3 'Anteprima contenuto'
163900000616     O          E            rigadett    2
164000000613     O                       rrnum               +2
164100000621     O                       recko               +3
164200000616     O          E            findett     2
164300000613     O                       wrkdata
164400000613     O                       wrkora              +1
164500000613     O                       procname            +1
164600000616     O          E            findett     2
164700000613     O                                              'Cliente..................:'
164800010605     O                       VABCCM        z     +1
164900000616     O          E            findett     2
165000000613     O                                              'Riferimento Strategi.....:'
165100000613     O                       vlrhdl              +1
165200000616     O          E            findett     2
165300000613     O                                              'Giusti...................:'
165400040119     O                       §CTROKVB      2   +  1
165500000616     O          E            findett     2
165600000613     O                                              'Sbagliati e corretti.....:'
165700000613     O                       §CTRMO        2   +  1
165800000616     O          E            findett     2
165900000613     O                                              'Sbagliati e scartati.....:'
166000000613     O                       §CTRNO        2   +  1
166100000613** CMD - COMANDI CL
166200050330OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
166300050330OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
166400050330DLTOVR FILE(TITVL6P TITVL6PS) LVL(*)
166500010606
166600000613
166700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
166800050330VABNSP;VABRMN;X;VABRSD;VABIND;X;CAPLOC;VABPKB;X;X
166900050330
167000040113
