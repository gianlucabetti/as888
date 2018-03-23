000100060703      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700030715     Ftitvxlp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030715     Ftitvxlps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600040726     D*------------
001700040726     D* SCHIEDE A PROGRAMMA
001800040726     D*------------
001900060215     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002000010330     D*----------------------------------------------------
002100010330     D* DICHIARAZIOINE VARIABILI DI WRK
002200010330     D*----------------------------------------------------
002300010330     D dscmz         e ds                  inz
002400010330     D psds           sds
002500010330     D  procname         *PROC
002600010330     D tivlrds       e ds                  extname(tivlr00f)
002700060306     D tisi95ds      e ds
002800010330     D esito           s              1
002900010330     D prmlit          s             10
003000010330     D prmfir          s             10
003100010330     D wrkesito        s                   like(esito)
003200010330     D wrkdata         s               d
003300010330     D wrkora          s               t
003400010330     D rrnum           s              6  0 INZ(*zeros)
003500010330     D recko           s            150    INZ(*blanks)
003600011113     D depcmd          s            150    INZ(*blanks)
003700060328     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003800060704     D saltaRec        s              1  0 INZ(1)
003900030715     D*------------------
004000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004100030715     D*------------------
004200030715     D SkSplitFLD      S             10    DIM(100)
004300030715     D SkSplitCSV      S            256    DIM(100)
004400030715     D CharCSV         S              1
004500030715     D CharTXT         S              1
004600030715     D CharNUM         S              1
004700030715     D posDa           S              3  0 INZ(*zeros)
004800030715     D posA            S              3  0 INZ(*zeros)
004900030715     D i               s              3  0 INZ(1)
005000030715     D wGiro           s              1  0 INZ(*zeros)
005100030822     D*------------------
005200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005300030822     D*------------------
005400030822     D posDaDft        S              3  0 INZ(*zeros)
005500030822     D posADft         S              3  0 INZ(*zeros)
005600030822     D j               s              3  0 INZ(1)
005700030822     D wGiroDft        s              1  0 INZ(*zeros)
005800010330     D*------------------
005900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006000010330     D*------------------
006100010330     D WLBDA8          DS                  INZ
006200010330     D  G08DAT                 1      8  0
006300010330     D  G08INV                 9     16  0
006400010330     D  G08ERR                17     17
006500010330     D  G08TGI                18     22  0
006600031201     D*------------------
006700031201     D* Costanti
006800031201     D*------------------
006900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007100010330
007200010330
007300010330
007400990915     C                   time                    wrkdata
007500990915     C                   time                    wrkora
007600000913     C                   reset                   rrnum
007700990921     C                   reset                   esito
007800990921     C                   reset                   wrkesito
007900010601     C*
008000010601     C                   exsr      opeini
008100010605     C                   exsr      rwvab
008200010601     C*
008300010601     C                   seton                                        lr
008400010601
008500010601
008600010601
008700010601
008800010601     C*--------------------------------------------------------
008900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009000010601     C*--------------------------------------------------------
009100010601     C     PREELA        BEGSR
009200010601     C*
009300010601     C* SE OCCORRE SPEDIRE IN FILIALE
009400010601     C                   if        invfil <> *zeros and
009500010601     C                             flgGiro = '0'
009600010601     C*
009700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009800010601     C                   eval      flgGiro = '1'
009900000724     C*
010000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010200010601     C     invfil        chain     azorg01l
010300000724     C                   if        %found
010400000724     C                   movel(p)  CMD(1)        depcmd
010500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010600000724     C*
010700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010800011113     C                   Z-ADD     150           LENGH            15 5
010900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011000000724     C                   PARM                    depcmd
011100000724     C                   PARM                    LENGH
011200000724     C*
011300000724     C                   endif
011400000616     C*
011500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011600000616     C                   movel(p)  CMD(2)        depcmd
011700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011800000616     C*
011900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    depcmd
012300000616     C                   PARM                    LENGH
012400000613     C*
012500030715     C                   if        not %open(titvxlps)
012600030715     C                   open      titvxlps
012700030715     C                   except    testdett
012800000613     C                   endif
012900010601     C*
013000010601     C                   endif
013100010601     C*
013200010601     C                   ENDSR
013300010601     C***
013400010601
013500010601
013600010601
013700010601     C*--------------------------------------------------------
013800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013900010601     C*--------------------------------------------------------
014000010601     C     ENDELA        BEGSR
014100990915     C*
014200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014300060306     C*
014400060306     C* Effettuo lancio TISI95 solo x chiusura
014500060306     C                   CLEAR                   TISI95DS
014600060306     C                   EVAL      I95TLA = 'C'
014700060306     C                   CALL      'TISI95R'
014800060306     C                   PARM                    TISI95DS
014900000613     C*
015000030715     C                   if        %open(titvxlps)
015100000616     C                   except    findett
015200030715     C                   close     titvxlps
015300000613     C                   endif
015400000616     C*
015500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015600011113     C                   Z-ADD     150           LENGH            15 5
015700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015800000616     C                   PARM                    CMD(3)
015900000616     C                   PARM                    LENGH
016000000616     C*
016100010601     C                   ENDSR
016200010601     C***
016300010601
016400010601
016500000613
016600000613     C*--------------------------------------------------------
016700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016800000613     C*--------------------------------------------------------
016900000621     C     STPR          BEGSR
017000000613     C*
017100030715     C                   if        not %open(titvxlp)
017200030715     C   50              open      titvxlp
017300990915     C                   endif
017400990915     C*
017500030715     C   50              except    riepilogo
017600990915     C*
017700030715     C                   if        %open(titvxlp)
017800030715     C   50              close     titvxlp
017900990914     C                   endif
018000000613     C*
018100000613     C                   ENDSR
018200000613     C***
018300010601
018400010601
018500010601
018600010330     C*--------------------------------------------------------
018700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
018800010330     C*--------------------------------------------------------
018900010605     C     RWVAB         BEGSR
019000010330     C*
019100010330     C                   if        not %open(tivin00r)
019200010330     C                   open      tivin00r
019300010330     C                   endif
019400021025     C                   if        not %open(fivabwwr)
019500021025     C                   open      fivabwwr
019600010330     C                   endif
019700010330     C*
019800010604     C                   clear                   §CTROK
019900010604     C                   clear                   §CTRMO
020000010604     C                   clear                   §CTRNO
020100060328     C*
020200060328     C* Eseguo subito inizializzazioni
020300060328     C                   exsr      inzvar
020400060328     C*
020500060328     C* Se necessario e quindi richiesto nei parametri d ingresso =>
020600060328     C* eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
020700040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
020800060704     C                   if        *in51 = *on
020900040726     C                   exsr      impvab
021000060328     C                   endif
021100010330     C*
021200030822     C                   do        *HIVAL
021300010330     C*
021400030822     C                   read      tivin00r                               70
021500060704     C*
021600060704     C* Elaboro solo dal 3° record in poi in quanto i primi 2 record sono da nn considerare
021700060704     C                   if        saltaRec < 3
021800060704     C                   add       1             saltaRec
021900060704     C                   eval      vinflg = '1'
022000060704     C                   else
022100010618     C*
022200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
022300010618     C                   if        vinflg = '1'
022400010618     C                   eval      flgOk = '1'
022500010618     C                   endif
022600060216     C*
022700060216     C* Se byte inferiore a HEX40 è potenzialmente probabile ricorra 1 errore nella visualizzazione
022800060216     C* => gestisco sempre
022900060216     C                   z-add     *zeros        wLenVINDTA        4 0
023000060216     C                   dow       wLenVINDTA < %len(%trim(vindta))
023100060216     C                   eval      wLenVINDTA = wLenVINDTA + 1
023200060216     C                   if        %subst(vindta:wLenVINDTA:1) < X'40'
023300060216     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
023400060216     C                   endif
023500060216     C                   enddo
023600010618     C*
023700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
023800040510     C                   z-add     *zeros        wLenVINDTA        4 0
023900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
024200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024300040510     C                   z-add     1             wFlgVINDTA
024400040510     C                   leave
024500040510     C                   endif
024600040510     C                   enddo
024700040510     C*
024800040218     C                   if        vindta > *blanks AND
024900040510     C                             wFlgVINDTA = 1
025000010330     C                   add       1             rrnum
025100010330     C*
025200010601     C                   if        *in70 = *off and
025300010330     C                             (vinflg = *blanks
025400010330     C                              or vinflg = '0'
025500010330     C                              or vinflg = '2')
025600010330     C*
025700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025800010711     C                   if        vinflg = *blanks or vinflg = '0'
025900010711     C                   clear                   vinmsg
026000010711     C                   endif
026100010601     C*
026200010330     C                   exsr      inzvar
026300010605     C                   exsr      impvab
026400010601     C*
026500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026600010605     C                   eval      depfil = VABLNP
026700010601     C                   exsr      repfil
026800010601     C                   if        depfil = invfil
026900021025     C                   if        vlrpoi = 999
027000030822     C                   move(P)   invfil        VABFGS
027100021025     C                   else
027200030822     C                   move(P)   vlrpoi        VABFGS
027300021025     C                   endif
027400010601     C*
027500010601     C                   exsr      PREELA
027600010601     C*
027700010604     C* Ebbene...
027800010604     C*
027900030822     C  N31              add       1             §CTROK            7 0
028000030822     C   32              add       1             §CTRMO            7 0
028100030822     C   31              add       1             §CTRNO            7 0
028200030822     C                   if        wGiro = 2
028300030822     C  N31              write     FIVAB000
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
030100060704     C                   endif
030200010601     C*
030300010601     C  N70              update    tivin000
030400010330     C*
030500030822     C  N70              enddo
030600010601     C
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
031800010330     C                   if        §ctrok > 0
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
033800010601     C*
033900010601     C                   if        vlrpoi <> 999
034000010601     C                   eval      invfil = vlrpoi
034100010601     C                   endif
034200010330     C*
034300010330     C                   if        §ctrok > 0
034400010601     C                             and invfil > *zeros
034500010330     C                   exsr      invio
034600010330     C                   endif
034700010601     C*
034800010618     C                   if        flgGiro = '1'
034900010601     C                   exsr      endela
035000010618     C                   endif
035100010330     C*
035200010330     C                   ENDSR
035300010330     C***
035400010601
035500010601
035600010601
035700010330     C*----------------------------------------------------*
035800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035900010330     C*----------------------------------------------------*
036000010330     C     INZVAR        BEGSR
036100010330     C*
036200030822     C* Inizializzo variabili di wrk
036300020204     C                   Z-ADD     *zeros        Num5_0            5 0
036400060703     C                   MOVEL     '0'           FlgCAS            1
036500030822     C*
036600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
036700030822     C                   CLEAR                   FIVAB000
036800030822     C                   CLEAR                   SkSplitCSV
036900030822     C                   EVAL      i = 1
037000030822     C                   EVAL      posDa = *zeros
037100030822     C                   EVAL      posA  = *zeros
037200030822     C*
037300030822     C* Reimposto i valori di default
037400030822     C                   EXSR      DEFCAM
037500030822     C*
037600010330     C                   ENDSR
037700010330     C*----------------------------------------------------*
037800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
037900010330     C*----------------------------------------------------*
038000010330     C     DEFCAM        BEGSR
038100060703     C*
038200060703     C* Imposto i valori d default a programma dei campi fondamentali
038300060703     C                   EVAL      VABCCM = 1100744
038400060703     C                   EVAL      VABLNP = 093
038500060703     C                   EVAL      VABCTR = 300
038600060703     C                   EVAL      VABCBO = '1'
038700060703     C                   EVAL      VABTSP = 'C'
038800060703     C                   EVAL      VABCTM = '7K'
038900030822     C*
039000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
039100030715     C* e delimitatore testo.
039200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
039300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
039400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
039500030822     C*
039600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
039700060328     C                   SETOFF                                       50
039800060328     C                   IF        %subst(vlrppt:1:1) = 'S'
039900030715     C                   SETON                                        50
040000030715     C                   ENDIF
040100060328     C                   IF        %subst(vlrppt:1:1) = 'I'
040200060328     C                   SETON                                        51
040300060328     C                   ENDIF
040400030822     C*
040500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
040600030822     C                   EVAL      posDaDft = 1
040700030822     C                   EVAL      posADft  = 0
040800030822     C                   EVAL      wGiroDft = 0
040900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
041000030822     C                             posDaDft > 0
041100030822     C*
041200030822     C* Gestisco il 1° giro
041300030822     C                   IF        wGiroDft = 0
041400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
041500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
041600030822     C* Incremento il contatore dei "giri"
041700030822     C                   EVAL      wGiroDft = 1
041800030822     C                   ELSE
041900030822     C                   EVAL      posDaDft = posADft
042000030822     C                   ENDIF
042100030822     C* Eseguo lo scan x trovare la fine del campo corrente
042200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
042300030822     C*
042400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
042500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
042600030822     C                   IF        posDaDft > 0 AND
042700030822     C                             posADft  > 0
042800030822     C* NCL
042900030822     C                   IF        %subst(
043000030822     C                             %subst(vlrppt:posDaDft+1:
043100030822     C                             posADft-posDaDft-1):1:3)
043200030822     C                             = 'NCL'
043300030822     C                   EVAL      PiStr=%trim(%subst(
043400030822     C                             %subst(vlrppt:posDaDft+1:
043500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043600030822     C                   EXSR      CHKNUM
043700030822     C                   IF        PiInt=*on
043800030822     C                   Z-ADD     PiVal         VABNCL
043900030822     C                   ENDIF
044000030822     C                   ENDIF
044100030822     C* CCM
044200030822     C                   IF        %subst(
044300030822     C                             %subst(vlrppt:posDaDft+1:
044400030822     C                             posADft-posDaDft-1):1:3)
044500030822     C                             = 'CCM'
044600030822     C                   EVAL      PiStr=%trim(%subst(
044700030822     C                             %subst(vlrppt:posDaDft+1:
044800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044900030822     C                   EXSR      CHKNUM
045000030822     C                   IF        PiInt=*on
045100030822     C                   Z-ADD     PiVal         VABCCM
045200030822     C                   ENDIF
045300030822     C                   ENDIF
045400030822     C* LNP
045500030822     C                   IF        %subst(
045600030822     C                             %subst(vlrppt:posDaDft+1:
045700030822     C                             posADft-posDaDft-1):1:3)
045800030822     C                             = 'LNP'
045900030822     C                   EVAL      PiStr=%trim(%subst(
046000030822     C                             %subst(vlrppt:posDaDft+1:
046100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046200030822     C                   EXSR      CHKNUM
046300030822     C                   IF        PiInt=*on
046400030822     C                   Z-ADD     PiVal         VABLNP
046500030822     C                   ENDIF
046600030822     C                   ENDIF
046700030822     C* NRS
046800030822     C                   IF        %subst(
046900030822     C                             %subst(vlrppt:posDaDft+1:
047000030822     C                             posADft-posDaDft-1):1:3)
047100030822     C                             = 'NRS'
047200030822     C                   EVAL      PiStr=%trim(%subst(
047300030822     C                             %subst(vlrppt:posDaDft+1:
047400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047500030822     C                   EXSR      CHKNUM
047600030822     C                   IF        PiInt=*on
047700030822     C                   Z-ADD     PiVal         VABNRS
047800030822     C                   ENDIF
047900030822     C                   ENDIF
048000030822     C* CTR
048100030822     C                   IF        %subst(
048200030822     C                             %subst(vlrppt:posDaDft+1:
048300030822     C                             posADft-posDaDft-1):1:3)
048400030822     C                             = 'CTR'
048500030822     C                   EVAL      PiStr=%trim(%subst(
048600030822     C                             %subst(vlrppt:posDaDft+1:
048700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048800030822     C                   EXSR      CHKNUM
048900030822     C                   IF        PiInt=*on
049000030822     C                   Z-ADD     PiVal         VABCTR
049100030822     C                   ENDIF
049200030822     C                   ENDIF
049300030822     C* PKB
049400030822     C                   IF        %subst(
049500030822     C                             %subst(vlrppt:posDaDft+1:
049600030822     C                             posADft-posDaDft-1):1:3)
049700030822     C                             = 'PKB'
049800030822     C                   EVAL      PiStr=%trim(%subst(
049900030822     C                             %subst(vlrppt:posDaDft+1:
050000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050100030822     C                   EXSR      CHKNUM
050200030822     C                   IF        PiNum=*on
050300030822     C                   Z-ADD     PiVal         VABPKB
050400030822     C                   ENDIF
050500030822     C                   ENDIF
050600030822     C* VLB
050700030822     C                   IF        %subst(
050800030822     C                             %subst(vlrppt:posDaDft+1:
050900030822     C                             posADft-posDaDft-1):1:3)
051000030822     C                             = 'VLB'
051100030822     C                   EVAL      PiStr=%trim(%subst(
051200030822     C                             %subst(vlrppt:posDaDft+1:
051300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051400030822     C                   EXSR      CHKNUM
051500030822     C                   IF        PiNum=*on
051600030822     C                   Z-ADD     PiVal         VABVLB
051700030822     C                   ENDIF
051800030822     C                   ENDIF
051900030822     C* QFT
052000030822     C                   IF        %subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):1:3)
052300030822     C                             = 'QFT'
052400030822     C                   EVAL      PiStr=%trim(%subst(
052500030822     C                             %subst(vlrppt:posDaDft+1:
052600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052700030822     C                   EXSR      CHKNUM
052800030822     C                   IF        PiNum=*on
052900030822     C                   Z-ADD     PiVal         VABQFT
053000030822     C                   ENDIF
053100030822     C                   ENDIF
053200030822     C* CBO
053300030822     C                   IF        %subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):1:3)
053600030822     C                             = 'CBO'
053700030822     C                   EVAL      VABCBO=%trim(%subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054000030822     C                   ENDIF
054100030822     C* TSP
054200030822     C                   IF        %subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):1:3)
054500030822     C                             = 'TSP'
054600030822     C                   EVAL      VABTSP=%trim(%subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054900030822     C                   ENDIF
055000030822     C* VAS
055100030822     C                   IF        %subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):1:3)
055400030822     C                             = 'VAS'
055500030822     C                   EVAL      VABVAS=%trim(%subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055800030822     C                   ENDIF
055900030822     C* VCA
056000030822     C                   IF        %subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):1:3)
056300030822     C                             = 'VCA'
056400030822     C                   EVAL      VABVCA=%trim(%subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056700030822     C                   ENDIF
056800030822     C* GCA
056900030822     C                   IF        %subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):1:3)
057200030822     C                             = 'GCA'
057300030822     C                   EVAL      VABGCA=%trim(%subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057600030822     C                   ENDIF
057700030822     C* CTM
057800030822     C                   IF        %subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):1:3)
058100030822     C                             = 'CTM'
058200030822     C                   EVAL      VABCTM=%trim(%subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500030822     C                   ENDIF
058600030822     C* FFD
058700030822     C                   IF        %subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):1:3)
059000030822     C                             = 'FFD'
059100030822     C                   EVAL      VABFFD=%trim(%subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400030822     C                   ENDIF
059500030822     C* VAD
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'VAD'
060000030822     C                   EVAL      VABVAD=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   ENDIF
060400030822     C* GMA
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'GMA'
060900030822     C                   EVAL      VABGMA=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   ENDIF
061300030822     C* GGA
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'GGA'
061800030822     C                   EVAL      VABGGA=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   ENDIF
062200030822     C* GVA
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'GVA'
062700030822     C                   EVAL      VABGVA=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   ENDIF
063100030822     C* TC1
063200030822     C                   IF        %subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):1:3)
063500030822     C                             = 'TC1'
063600030822     C                   EVAL      VABTC1=%trim(%subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900030822     C                   ENDIF
064000030822     C* TC2
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'TC2'
064500030822     C                   EVAL      VABTC2=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   ENDIF
064900030822     C* ...
065000030822     C                   ENDIF
065100030822     C                   ENDDO
065200020204     C*
065300010330     C                   ENDSR
065400010607     C*----------------------------------------------------*
065500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
065600010607     C*----------------------------------------------------*
065700010607     C     IMPVAB        BEGSR
065800010607     C*
065900010607     C                   SETOFF                                       3132
066000030515     C*
066100010607     C                   MOVEL     datcor        VABAAS
066200010607     C                   MOVE      datcor        VABMGS
066300050321     C*
066400050512     C* 1° Giro x eliminare il separatore testo
066500060215     C                   IF        CharTXT <> *blanks
066600050321     C                   Z-ADD     1             wj                5 0
066700050321     C                   DOW       wj <= %len(vindta) - 1
066800050321     C                   IF        %subst(vindta:wj:1) = CharTXT
066900050512     C                   EVAL      vindta = %subst(vindta:1:wj-1) +
067000050512     C                                      %subst(vindta:wj+1)
067100050512     C                   EVAL      wj = wj - 1
067200050512     C*                  EVAL      %subst(vindta:wj:1) = *blanks
067300050321     C                   ENDIF
067400050321     C                   ADD       1             wj
067500050321     C                   ENDDO
067600060215     C                   ENDIF
067700050512     C*
067800060215     C* Se la stringa dati da splittare inizia con il carattere separatore campo aggiungo in testa
067900050512     C* 2 byte a blank
068000050512     C                   IF        %subst(vindta:1:1) = CharCSV
068100050512     C                   EVAL      vindta = '  ' + vindta
068200050512     C                   ENDIF
068300010607     C*
068400030715     C**********
068500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
068600030715     C**********
068700030715     C                   IF        wGiro = *zeros
068800040726     C*
068900040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
069000040726     C                   EVAL      depvinDTA = vinDTA
069100040726     C*
069200040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
069300040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
069400050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
069500060215     C                                      %trim(intcol(5))
069600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
069700031201     C     minu:maiu     XLATE     vindta        vindta
069800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
069900040324     C                   DOW       posDa <= %len(%trimr(vindta))
070000030715     C*
070100030715     C* Gestisco il 1° campo
070200030715     C                   IF        i = 1
070300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
070400030715     C                   EVAL      posDa = 1
070500030715     C* Eseguo lo scan x trovare la fine del primo campo
070600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
070700030715     C* A questo punto "estraggo" il campo corrente
070800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070900030715     C                                                    (posA-posDa))
071000030715     C* X i campi successivi al 1°
071100030715     C                   ELSE
071200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
071300030715     C                   EVAL      posDa = posA + 1
071400030715     C* Eseguo lo scan x trovare la fine del campo corrente
071500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071600030715     C* Gestisco l'ultimo campo
071700030715     C                   IF        posA = *zeros
071800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
071900030715     C* A questo punto "estraggo" il campo corrente
072000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
072100030715     C                                                    (posA-posDa))
072200030715     C                   LEAVE
072300030715     C                   ELSE
072400030715     C* A questo punto "estraggo" il campo corrente
072500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
072600030715     C                                                    (posA-posDa))
072700030715     C                   ENDIF
072800030715     C                   ENDIF
072900030715     C* Incremento il contatore d campo
073000030715     C                   EVAL      i = i +1
073100030715     C                   ENDDO
073200030715     C*
073300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
073400030715     C* delimitazione testo
073500030715     C                   EVAL      i = 1
073600030715     C                   DOW       i < %elem(SkSplitFLD)
073700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
073800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
073900030715     C                   EVAL      i = i + 1
074000030715     C                   ENDDO
074100030715     C                   EVAL      wGiro = 1
074200040726     C* Ripristino la 1° riga originale
074300040726     C                   EVAL      vinDTA = depvinDTA
074400030715     C                   ELSE
074500030715     C                   EVAL      wGiro = 2
074600031204     C**********
074700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
074800031204     C* d separatore campo
074900031204     C**********
075000060215     C                   EVAL      vindta = %trim(vindta)
075100060215     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
075200060215     C                   EVAL      vindta = %subst(vindta:2)
075300060215     C                   ENDDO
075400060215     C
075500060215     C                   Z-ADD     *zeros        lunghInput        4 0
075600060215     C                   EVAL      lunghInput = %len(%trimr(vindta))
075700060215     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
075800060215     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
075900060215     C                   EVAL      lunghInput = %len(%trim(vindta))
076000060215     C                   ENDDO
076100030715     C**********
076200030715     C* Effettuo lo split del campo dati d input
076300030715     C**********
076400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
076500040324     C                   DOW       posDa <= %len(%trimr(vindta))
076600030715     C*
076700030715     C* Gestisco il 1° campo
076800030715     C                   IF        i = 1
076900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
077000030715     C                   EVAL      posDa = 1
077100030715     C* Eseguo lo scan x trovare la fine del primo campo
077200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077300030715     C* A questo punto "estraggo" il campo corrente
077400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
077500030715     C                                                    (posA-posDa))
077600030715     C* X i campi successivi al 1°
077700030715     C                   ELSE
077800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
077900030715     C                   EVAL      posDa = posA + 1
078000031204     C* Verifico che nn vi sia il campo nn valorizzato
078100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
078200031204     C* Se campo nn valorizzato skippo al prossimo
078300031204     C                   EVAL      SkSplitCSV(i) = *blanks
078400031204     C                   EVAL      posA  = posA + 1
078500031204     C                   ELSE
078600030715     C* Eseguo lo scan x trovare la fine del campo corrente
078700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078800030715     C* Gestisco l'ultimo campo
078900030715     C                   IF        posA = *zeros
079000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
079100030715     C* A questo punto "estraggo" il campo corrente
079200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079300030715     C                                                    (posA-posDa))
079400030715     C                   LEAVE
079500030715     C                   ELSE
079600030715     C* A questo punto "estraggo" il campo corrente
079700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079800030715     C                                                    (posA-posDa))
079900030715     C                   ENDIF
080000031204     C                   ENDIF
080100030715     C                   ENDIF
080200030715     C* Incremento il contatore d campo
080300030715     C                   EVAL      i = i +1
080400030715     C                   ENDDO
080500030715     C*
080600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
080700030715     C* delimitazione testo
080800030715     C                   EVAL      i = 1
080900030715     C                   DOW       i < %elem(SkSplitCSV)
081000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
081100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
081200030715     C                   EVAL      i = i + 1
081300030715     C                   ENDDO
081400030715     C*
081500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
081600030715     C                   EVAL      i = 1
081700030715     C                   DOW       i < %elem(SkSplitFLD)
081800030715     C*
081900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
082000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
082100030715     C                             SkSplitCSV(i) <> *blanks
082200030721     C*** ATB
082300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
082400030721     C                   EVAL      VABATB = SkSplitCSV(i)
082500030721     C                   ENDIF
082600030715     C*** RSD
082700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
082800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
082900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
083000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
083100030715     C* ==
083200030715     C                   ENDIF
083300030721     C*** RD2
083400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
083500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
083600030721     C                   ENDIF
083700030715     C*** IND
083800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
083900030715     C                   EVAL      VABIND = SkSplitCSV(i)
084000030715     C                   ENDIF
084100030715     C*** LOD
084200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
084300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
084400030715     C                   ENDIF
084500030715     C*** PRD
084600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
084700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
084800030715     C                   ENDIF
084900030721     C*** NZD
085000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
085100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
085200030721     C                   ENDIF
085300030715     C*** RMA
085400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
085500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
085600030715     C                   ENDIF
085700030715     C*** NOT
085800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
085900060306     C                   EVAL      VABNOT = SkSplitCSV(i)
086000030715     C                   ENDIF
086100030715     C*** NT2
086200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
086300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
086400030715     C                   ENDIF
086500030715     C*** VCA
086600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
086700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
086800030715     C                   ENDIF
086900030715     C*** VAS
087000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
087100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
087200030715     C                   ENDIF
087300030715     C*** FFD
087400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
087500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
087600030715     C                   ENDIF
087700030715     C*** CBO
087800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
087900060703     C                   IF        SkSplitCSV(i) = 'PF'
088000060703     C                   EVAL      VABCBO = '1'
088100060703     C                   ENDIF
088200060703     C                   IF        SkSplitCSV(i) = 'PA'
088300060703     C                   EVAL      VABCBO = '2'
088400060703     C                   ENDIF
088500030715     C                   ENDIF
088600030721     C*** GC1
088700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
088800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
088900030721     C                   ENDIF
089000030721     C*** GC2
089100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
089200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
089300030721     C                   ENDIF
089400030721     C*** TSP
089500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
089600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
089700030721     C                   ENDIF
089800030721     C*** NAS
089900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
090000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
090100030721     C                   ENDIF
090200030721     C*** TIC
090300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
090400060703     C                   IF        SkSplitCSV(i) <> '0'
090500060703     C                   EVAL      FlgCAS = '1'
090600060703     C                   ENDIF
090700060703     C                   IF        SkSplitCSV(i) = '1'
090800060703     C                   EVAL      VABTIC = 'TM'
090900060703     C                   ENDIF
091000060703     C                   IF        SkSplitCSV(i) = '2'
091100060703     C                   EVAL      VABTIC = *blanks
091200060703     C                   ENDIF
091300060703     C                   IF        SkSplitCSV(i) = '3'
091400060703     C                   EVAL      VABTIC = 'BM'
091500060703     C                   ENDIF
091600030721     C                   ENDIF
091700030721     C*** GCA
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
091900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030721     C*** XCO
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
092300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
092400030721     C                   ENDIF
092500030721     C*** CTM
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
092700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030721     C*** TCR
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
093100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
093200030721     C                   ENDIF
093300030721     C*** CTS
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
093500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
093600030721     C                   ENDIF
093700030721     C*** FTM
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
093900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030721     C*** VAD
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
094300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030721     C*** GMA
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
094700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030721     C*** GGA
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
095100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
095200030721     C                   ENDIF
095300030721     C*** GVA
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
095500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030721     C*** TC1
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
095900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030721     C*** TC2
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
096300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030721     C*** SCL
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
096700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030721     C*** RMO
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
097100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
097200030721     C                   ENDIF
097300030721     C*** NMO
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
097500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030721     C*
097800030721     C* Reperisco quindi i campi numerici...
097900030721     C*** CAD
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
098100030721     C                   EVAL      PiStr=SkSplitCSV(i)
098200030721     C                   EXSR      CHKNUM
098300030721     C                   IF        PiInt=*on
098400030721     C                   Z-ADD     PiVal         Num5_0
098500030721     C                   MOVEL(p)  Num5_0        VABCAD
098600030721     C                   ELSE
098700030721     C                   SETON                                        32
098800030721     C                   EVAL      VABCAD = *zeros
098900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099000030721     C                             + ' ' + 'VABCAD'
099100030721     C                   ENDIF
099200030721     C                   ENDIF
099300030721     C*** RMN
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
099500060703     C                   EVAL      PiStr=%subst(SkSplitCSV(i):1:6)
099600030721     C                   EXSR      CHKNUM
099700030721     C                   IF        PiInt=*on
099800030721     C                   Z-ADD     PiVal         VABRMN
099900030721     C                   ELSE
100000030721     C                   SETON                                        32
100100030721     C                   EVAL      VABRMN = 1
100200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100300030721     C                             + ' ' + 'VABRMN'
100400030721     C                   ENDIF
100500030721     C                   ENDIF
100600030721     C*** NSP
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
100800030721     C                   EVAL      PiStr=SkSplitCSV(i)
100900030721     C                   EXSR      CHKNUM
101000040726     C                   IF        PiInt=*on
101100040726     C                   Z-ADD     PiVal         VABNSP
101200040802     C*                  IF        PiVal>9999999
101300040802     C*                  SETON                                        32
101400040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
101500040802     C*                            + ' ' + 'VABNSP'
101600040802     C*                  ENDIF
101700030721     C                   ELSE
101800030721     C                   SETON                                        32
101900030721     C                   EVAL      VABNSP = *zeros
102000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102100030721     C                             + ' ' + 'VABNSP'
102200030721     C                   ENDIF
102300030721     C                   ENDIF
102400030721     C*** NCL
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
102600030721     C                   EVAL      PiStr=SkSplitCSV(i)
102700060703     C                   EVAL      VABRMA=PiStr
102800030721     C                   EXSR      CHKNUM
102900030721     C                   IF        PiInt=*on
103000030721     C                   Z-ADD     PiVal         VABNCL
103100030721     C                   ELSE
103200030721     C                   SETON                                        32
103300030721     C                   EVAL      VABNCL = *zeros
103400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103500030721     C                             + ' ' + 'VABNCL'
103600030721     C                   ENDIF
103700030721     C                   ENDIF
103800030721     C*** PKB
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
104000030721     C                   EVAL      PiStr=SkSplitCSV(i)
104100030721     C                   EXSR      CHKNUM
104200030721     C                   IF        PiNum=*on
104300030721     C                   Z-ADD     PiVal         VABPKB
104400030721     C                   ELSE
104500030721     C                   SETON                                        32
104600030721     C                   EVAL      VABPKB = *zeros
104700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104800030721     C                             + ' ' + 'VABPKB'
104900030721     C                   ENDIF
105000030721     C                   ENDIF
105100030721     C*** CAS
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
105300030721     C                   EVAL      PiStr=SkSplitCSV(i)
105400030721     C                   EXSR      CHKNUM
105500030721     C                   IF        PiNum=*on
105600030721     C                   Z-ADD     PiVal         VABCAS
105700030721     C                   ELSE
105800030721     C                   SETON                                        32
105900030721     C                   EVAL      VABCAS = *zeros
106000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106100030721     C                             + ' ' + 'VABCAS'
106200030721     C                   ENDIF
106300030721     C                   ENDIF
106400030721     C*** CCM
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
106600030721     C                   EVAL      PiStr=SkSplitCSV(i)
106700030721     C                   EXSR      CHKNUM
106800030721     C                   IF        PiInt=*on      AND
106900030721     C                             PiVal<=9999999 AND
107000030721     C                             PiVal>*zeros
107100030721     C                   Z-ADD     PiVal         VABCCM
107200030721     C                   ELSE
107300030721     C                   SETON                                        32
107400030721     C                   EVAL      VABCCM = *zeros
107500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107600030721     C                             + ' ' + 'VABCCM'
107700030721     C                   ENDIF
107800030721     C                   ENDIF
107900030721     C*** LNP
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
108100030721     C                   EVAL      PiStr=SkSplitCSV(i)
108200030721     C                   EXSR      CHKNUM
108300030721     C                   IF        PiInt=*on    AND
108400030721     C                             PiVal<=999   AND
108500030721     C                             PiVal>*zeros
108600030721     C                   Z-ADD     PiVal         VABLNP
108700030721     C                   ELSE
108800030721     C                   SETON                                        32
108900030721     C                   EVAL      VABLNP = *zeros
109000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109100030721     C                             + ' ' + 'VABLNP'
109200030721     C                   ENDIF
109300030721     C                   ENDIF
109400030721     C*** NRS
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
109600030721     C                   EVAL      PiStr=SkSplitCSV(i)
109700030721     C                   EXSR      CHKNUM
109800030721     C                   IF        PiInt=*on AND
109900030721     C                             PiVal<=99
110000030721     C                   Z-ADD     PiVal         VABNRS
110100030721     C                   ELSE
110200030721     C                   SETON                                        32
110300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110400030721     C                             + ' ' + 'VABNRS'
110500030721     C                   ENDIF
110600030721     C                   ENDIF
110700030721     C*** LNA
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
110900030721     C                   EVAL      PiStr=SkSplitCSV(i)
111000030721     C                   EXSR      CHKNUM
111100030721     C                   IF        PiInt=*on    AND
111200030721     C                             PiVal<=999   AND
111300030721     C                             PiVal>*zeros
111400030721     C                   Z-ADD     PiVal         VABLNA
111500030721     C                   ELSE
111600030721     C                   SETON                                        32
111700030721     C                   EVAL      VABLNA = *zeros
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900030721     C                             + ' ' + 'VABLNA'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** CTR
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on  AND
112700030721     C                             PiVal<=999
112800030721     C                   Z-ADD     PiVal         VABCTR
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABCTR'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** IAS
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiNum=*on
114000030721     C                   Z-ADD     PiVal         VABIAS
114100030721     C                   ELSE
114200030721     C                   SETON                                        32
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400030721     C                             + ' ' + 'VABIAS'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** VLB
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiNum=*on
115200030721     C                   Z-ADD     PiVal         VABVLB
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600030721     C                             + ' ' + 'VABVLB'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900030721     C*** QFT
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiNum=*on
116400030721     C                   Z-ADD     PiVal         VABQFT
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800030721     C                             + ' ' + 'VABQFT'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** NCD
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on      AND
117600030721     C                             PiVal<=9999999
117700030721     C                   Z-ADD     PiVal         VABNCD
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABNCD'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** NCA
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiInt=*on      AND
118900030721     C                             PiVal<=9999999
119000030721     C                   Z-ADD     PiVal         VABNCA
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400030721     C                             + ' ' + 'VABNCA'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** ZNC
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiInt=*on AND
120200030721     C                             PiVal<=99
120300030721     C                   Z-ADD     PiVal         VABZNC
120400030721     C                   ELSE
120500030721     C                   SETON                                        32
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABZNC'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** DCR
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiInt=*on       AND
121500030721     C                             PiVal<=99999999
121600030721     C                   Z-ADD     PiVal         VABDCR
121700030721     C                   ELSE
121800030721     C                   SETON                                        32
121900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122000030721     C                             + ' ' + 'VABDCR'
122100030721     C                   ENDIF
122200030721     C                   ENDIF
122300030721     C*** HCR
122400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiInt=*on   AND
122800030721     C                             PiVal<=9999
122900030721     C                   Z-ADD     PiVal         VABHCR
123000030721     C                   ELSE
123100030721     C                   SETON                                        32
123200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123300030721     C                             + ' ' + 'VABHCR'
123400030721     C                   ENDIF
123500030721     C                   ENDIF
123600030721     C*** VMD
123700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
123800030721     C                   EVAL      PiStr=SkSplitCSV(i)
123900030721     C                   EXSR      CHKNUM
124000030721     C                   IF        PiNum=*on
124100030721     C                   Z-ADD     PiVal         VABVMD
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABVMD'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** ANT
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on        AND
125300030721     C                             PiVal<=999999999
125400030721     C                   Z-ADD     PiVal         VABANT
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABANT'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** CMO
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiInt=*on
126600030721     C                   Z-ADD     PiVal         Num5_0
126700030721     C                   MOVEL(p)  Num5_0        VABCMO
126800030721     C                   ELSE
126900030721     C                   SETON                                        32
127000030721     C                   EVAL      VABCMO = *zeros
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABCMO'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500100823     C*
127600100823     C***  ===>  Gestione campi "particolari" dentro ciclo
127700100823     C*** NT2 / CCM / CTR
127800100823     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNT2'
127900100823     C                   EVAL      VABNT2 = %trim(SkSplitCSV(i))
128000100823     C                   IF        %trim(SkSplitCSV(i)) = 'SAFE**'
128100100823     C                   EVAL      VABCCM = 0934104
128200100823     C                   EVAL      VABCTR = 100
128300100823     C                   ENDIF
128400100823     C                   ENDIF
128500100823     C*
128600100823     C***  <===  -----------------------------------------
128700030715     C*
128800030715     C                   ENDIF
128900030715     C                   EVAL      i = i + 1
129000030715     C                   ENDDO
129100100929     C*
129200100929     C***  ===>  Gestione "particolarità" fuori ciclo
129300100929     C                   IF        VABCCM = 0934104
129400100929     C                   EVAL      VABIAS = 1500 * VABNCL
129500100929     C                   EVAL      VABVAS = 'EUR'
129600100929     C                   ENDIF
129700100929     C*
129800100929     C***  <===  -----------------------------------------
129900060328     C*
130000060306     C* Reperisco la provincia dal CAP e dalla località
130100060306     C                   IF        VABPRD = *blanks AND
130200060306     C                             VABCAD <> *blanks
130300060306     C                   CLEAR                   TISI95DS
130400060306     C                   EVAL      I95TCN = '3'
130500060306     C                   Z-ADD     datcor        I95DAT
130600060306     C                   EVAL      I95CAP = VABCAD
130700060306     C                   EVAL      I95LOC = VABLOD
130800060306     C                   CALL      'TISI95R'
130900060306     C                   PARM                    TISI95DS
131000060306     C                   EVAL      VABPRD = O95PRV
131100060306     C                   ENDIF
131200040726     C*
131300040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
131400060328     C                   IF        VABNSP =  *zeros
131500060328     C                   IF        VABRMN <> *zeros
131600060328     C                   Z-ADD     VABRMN        VABNSP
131700060328     C                   ELSE
131800040726     C                   SETON                                        31
131900060328     C                   ENDIF
132000040726     C                   ENDIF
132100040726     C*
132200040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
132300040726     C                   IF        VABLNP = *zeros
132400040726     C                   SETON                                        31
132500040726     C                   ENDIF
132600040726     C*
132700040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
132800040726     C* d valorizzarli sempre entrambi
132900040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
133000040726     C                   Z-ADD     VABNSP        VABRMN
133100040726     C                   ENDIF
133200040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
133300040726     C                   MOVEL     VABRMN        VABRMA
133400040726     C                   ENDIF
133500040726     C*
133600040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
133700060703     C                   IF        FlgCAS <> '0'
133800040726     C                   IF        VABVCA = *blanks
133900060328     C                   EVAL      VABVCA = 'EUR'
134000040726     C                   ENDIF
134100040726     C                   IF        VABCBO = '1'
134200040726     C                   EVAL      VABCBO = '4'
134300040726     C                   ENDIF
134400040726     C                   IF        VABCBO = '2'
134500040726     C                   EVAL      VABCBO = '6'
134600040726     C                   ENDIF
134700040726     C                   ENDIF
134800020204     C*
134900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
135000020204     C                   EXSR      CHKIMPDIV
135100030715     C                   ENDIF
135200020204     C*
135300010607     C                   ENDSR
135400010607     C*----------------------------------------------------*
135500010601
135600020204
135700020204     C*----------------------------------------------------*
135800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
135900020204     C*----------------------------------------------------*
136000020204     C     CHKIMPDIV     BEGSR
136100020204     C*
136200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
136300020204     C                   Z-ADD     *zeros        wrkDec            9 9
136400020204     C*
136500020204     C* Come prima cosa effettuo considerazioni sulla divisa
136600020204     C                   IF        vabIAS > *zeros
136700020204     C                   IF        vabVAS <> 'EUR'
136800020204     C                   EVAL      vabVAS =  'ITL'
136900020204     C                   ENDIF
137000020204     C                   ENDIF
137100020204     C*
137200020204     C                   IF        vabCAS > *zeros
137300020204     C                   IF        vabVCA <> 'EUR'
137400020204     C                   EVAL      vabVCA =  'ITL'
137500020204     C                   ENDIF
137600020204     C                   ENDIF
137700020204     C*
137800020204     C                   IF        vabVMD > *zeros
137900020204     C                   IF        vabVAD <> 'EUR'
138000020204     C                   EVAL      vabVAD =  'ITL'
138100020204     C                   ENDIF
138200020204     C                   ENDIF
138300020204     C*
138400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
138500020204     C                   Z-ADD     vabIAS        wrkDec
138600020204     C                   IF        wrkDec > *zeros
138700020204     C                   IF        vabVAS = 'ITL'
138800020204     C                   EVAL      vabIAS = *zeros
138900020204     C                   ENDIF
139000020204     C                   ENDIF
139100020204     C*
139200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
139300020204     C                   Z-ADD     vabCAS        wrkDec
139400020204     C                   IF        wrkDec > *zeros
139500020204     C                   IF        vabVCA = 'ITL'
139600020204     C                   EVAL      vabCAS = *zeros
139700020204     C                   ENDIF
139800020204     C                   ENDIF
139900020204     C*
140000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
140100020204     C                   Z-ADD     vabVMD        wrkDec
140200020204     C                   IF        wrkDec > *zeros
140300020204     C                   IF        vabVAD = 'ITL'
140400020204     C                   EVAL      vabVMD = *zeros
140500020204     C                   ENDIF
140600020204     C                   ENDIF
140700020204     C*
140800020204     C                   ENDSR
140900020204     C***
141000020204
141100010330
141200010330
141300010330     C*----------------------------------------------------*
141400010330     C*  CONTROLLO NUMERICITA' CAMPI
141500010330     C*----------------------------------------------------*
141600010330     C     CHKNUM        BEGSR
141700010330     C*
141800010606     C                   IF        PiDecChr = *blanks
141900030715     C                   EVAL      PiDecChr = CharNUM
142000010606     C                   ENDIF
142100010606     C*
142200010606     C                   CALL(e)   'ISNUMERIC'
142300010330     C                   PARM                    PiStr            30
142400010606     C                   PARM                    PiDecChr          1
142500010330     C                   PARM      *ZEROS        PiVal            30 9
142600010330     C                   PARM      '0'           PiInt             1
142700010330     C                   PARM      '0'           PiNum             1
142800010330     C                   IF        %error
142900010606     C                   EVAL      PiNum=*off
143000010330     C                   ENDIF
143100010330     C*
143200010330     C                   ENDSR
143300010330     C***
143400010330
143500010601
143600010601
143700010601
143800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
143900010601     C     repfil        BEGSR
144000010601     C*
144100010601     C                   if        invfil = *zeros and
144200010601     C                             depfil > *zeros and
144300010629     C                             (vinflg = *blanks or
144400010629     C                              vinflg = *zeros)
144500010601     C
144600010601     C                   eval      invfil = depfil
144700010601     C                   endif
144800010601     C*
144900010601     C                   if        depfil <> invfil and
145000010601     C                             invfil > *zeros
145100010601     C                   eval      flgMulti = '1'
145200010601     C                   if        vinflg = *blanks
145300010601     C                   add       1             cntNonEl
145400010601     C                   endif
145500010601     C                   endif
145600010601     C*
145700010601     C                   if        vinflg = '2'
145800010601     C                   eval      flgStato = '2'
145900010601     C                   endif
146000010601     C*
146100010601     C                   ENDSR
146200010601     C***
146300010601
146400010330
146500010330
146600010330
146700990920      /TITLE Invio dei dati al punto operativo.
146800000613     C     invio         BEGSR
146900990920     C*
147000990920     C                   reset                   dscmz
147100010601     C                   move      invfil        cmzdst
147200990920     C                   eval      cmzfld = vlrfou
147300990920     C                   eval      cmzmbd = vlrhdl
147400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
147500000710     C                   if        prmfir = *blanks
147600021025     C                   eval      cmzfla = 'FIVAB00F'
147700021025     C                   eval      cmzmba = 'FIVAB00F'
147800000710     C                   else
147900000710     C                   eval      cmzfla = prmfir
148000000710     C                   eval      cmzmba = prmfir
148100000710     C                   endif
148200990920     C                   eval      cmznrr = *zeros
148300990920     C                   move      §ctrok        cmznrr
148400021018     C                   eval      cmzlba = vlrfl1
148500990920     C                   call(e)   'TIS711C'
148600990920     C                   parm                    dscmz
148700990921     C                   parm      *blanks       esito
148800990920     C                   if        %error
148900990920     C                             or cmzerr = '1'
149000990921     C                             or esito  = '1'
149100000710     C                   eval      wrkesito = '3'
149200990920     C                   endif
149300990920     C*
149400000613     C                   ENDSR
149500990910
149600010601
149700010601
149800010601
149900010601
150000010601      /TITLE Invio dei dati al punto operativo.
150100010601     C     opeini        BEGSR
150200010601     C*
150300010601     C* Inizializzo flag e contatori operativi
150400010601     C                   movel     '0'           flgGiro           1
150500010601     C                   movel     '0'           flgMulti          1
150600010601     C                   movel     '1'           flgStato          1
150700010615     C                   movel     '0'           flgOk             1
150800010601     C                   z-add     *zeros        cntNonEl         10 0
150900010601     C                   z-add     *zeros        depfil            3 0
151000010601     C                   z-add     *zeros        invfil            3 0
151100010601     C*
151200010601     C                   ENDSR
151300010601     C***
151400010601
151500010601
151600050421
151700050421
151800050421     C     *pssr         BEGSR
151900050421     C*
152000050421     C                   eval      wrkesito = '2'
152100050421     C*
152200050421     C                   ENDSR     '*CANCL'
152300050421     C***
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
154800990908
154900030715     Otitvxlp   E            riepilogo         2
155000990915     O                                              'Upload via Internet'
155100990915     O                                           +1 'Traduzione TIVIN00R -'
155200021025     O                                           +1 'FIVABWWR'
155300990915     O                                           +1 'Testate bolle'
155400990915     O          E            riepilogo   2
155500990915     O                       wrkdata
155600990915     O                       wrkora              +1
155700990915     O                       procname            +1
155800990915     O          E            riepilogo   2
155900990915     O                                              'Cliente..................:'
156000010605     O                       VABCCM        z     +1
156100990915     O          E            riepilogo   2
156200990920     O                                              'Riferimento Strategi.....:'
156300990920     O                       vlrhdl              +1
156400990915     O          E            riepilogo   2
156500990915     O                                              'Giusti...................:'
156600971022     O                       §CTROK        2   +  1
156700990915     O          E            riepilogo   2
156800990915     O                                              'Sbagliati e corretti.....:'
156900971022     O                       §CTRMO        2   +  1
157000990915     O          E            riepilogo   2
157100990915     O                                              'Sbagliati e scartati.....:'
157200971022     O                       §CTRNO        2   +  1
157300000613
157400030715     Otitvxlps  E            testdett          2
157500000613     O                                              'Upload via Internet'
157600000613     O                                           +1 'Traduzione TIVIN00R -'
157700021025     O                                           +1 'FIVABWWR'
157800000613     O                                           +1 'Testate bolle'
157900000616     O          E            testdett    3
158000000613     O                                           +2 'N° rec'
158100000613     O                                           +3 'Anteprima contenuto'
158200000616     O          E            rigadett    2
158300000613     O                       rrnum               +2
158400000621     O                       recko               +3
158500000616     O          E            findett     2
158600000613     O                       wrkdata
158700000613     O                       wrkora              +1
158800000613     O                       procname            +1
158900000616     O          E            findett     2
159000000613     O                                              'Cliente..................:'
159100010605     O                       VABCCM        z     +1
159200000616     O          E            findett     2
159300000613     O                                              'Riferimento Strategi.....:'
159400000613     O                       vlrhdl              +1
159500000616     O          E            findett     2
159600000613     O                                              'Giusti...................:'
159700000613     O                       §CTROK        2   +  1
159800000616     O          E            findett     2
159900000613     O                                              'Sbagliati e corretti.....:'
160000000613     O                       §CTRMO        2   +  1
160100000616     O          E            findett     2
160200000613     O                                              'Sbagliati e scartati.....:'
160300000613     O                       §CTRNO        2   +  1
160400000613** CMD - COMANDI CL
160500030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
160600030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
160700030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
160800010606
160900000613
161000040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
161100060703VABRMN;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;
161200100823VABCBO;VABTIC;VABCAS;W_VABNT2;VABNOT
161300050321
161400050321
161500060215
