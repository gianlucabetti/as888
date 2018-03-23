000100101018      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200130220     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600101018     FEDIVABwr  O    E             DISK    usropn
000700030715     Ftitvxlp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030715     Ftitvxlps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300080222     D tisi95ds      e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300030715     D*------------------
003400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003500030715     D*------------------
003600030715     D SkSplitFLD      S             10    DIM(100)
003700030715     D SkSplitCSV      S            256    DIM(100)
003800030715     D CharCSV         S              1
003900030715     D CharTXT         S              1
004000030715     D CharNUM         S              1
004100090403     D CharSOS         S              1
004200030715     D posDa           S              3  0 INZ(*zeros)
004300030715     D posA            S              3  0 INZ(*zeros)
004400030715     D i               s              3  0 INZ(1)
004500030715     D wGiro           s              1  0 INZ(*zeros)
004600030822     D*------------------
004700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004800030822     D*------------------
004900030822     D posDaDft        S              3  0 INZ(*zeros)
005000030822     D posADft         S              3  0 INZ(*zeros)
005100030822     D j               s              3  0 INZ(1)
005200030822     D wGiroDft        s              1  0 INZ(*zeros)
005300031201     D*------------------
005400031201     D* Costanti
005500031201     D*------------------
005600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005800010330
005900010330
006000990915     C                   time                    wrkdata
006100990915     C                   time                    wrkora
006200000913     C                   reset                   rrnum
006300990921     C                   reset                   esito
006400990921     C                   reset                   wrkesito
006500010601     C*
006600010601     C                   exsr      opeini
006700010605     C                   exsr      rwvab
006800010601     C*
006900010601     C                   seton                                        lr
007000010601
007100010601
007200010601
007300010601
007400010601     C*--------------------------------------------------------
007500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007600010601     C*--------------------------------------------------------
007700010601     C     PREELA        BEGSR
007800010601     C*
007900010601     C* SE OCCORRE SPEDIRE IN FILIALE
008000010601     C                   if        invfil <> *zeros and
008100010601     C                             flgGiro = '0'
008200010601     C*
008300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008400010601     C                   eval      flgGiro = '1'
008500000724     C*
008600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800010601     C     invfil        chain     azorg01l
008900000724     C                   if        %found
009000000724     C                   movel(p)  CMD(1)        depcmd
009100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009200000724     C*
009300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009400011113     C                   Z-ADD     150           LENGH            15 5
009500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009600000724     C                   PARM                    depcmd
009700000724     C                   PARM                    LENGH
009800000724     C*
009900000724     C                   endif
010000000616     C*
010100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010200000616     C                   movel(p)  CMD(2)        depcmd
010300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010400000616     C*
010500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010600011113     C                   Z-ADD     150           LENGH            15 5
010700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010800000616     C                   PARM                    depcmd
010900000616     C                   PARM                    LENGH
011000000613     C*
011100030715     C                   if        not %open(titvxlps)
011200030715     C                   open      titvxlps
011300030715     C                   except    testdett
011400000613     C                   endif
011500010601     C*
011600010601     C                   endif
011700010601     C*
011800010601     C                   ENDSR
011900010601     C***
012000010601
012100010601
012200010601
012300010601
012400010601
012500010601
012600010601
012700010601     C*--------------------------------------------------------
012800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012900010601     C*--------------------------------------------------------
013000010601     C     ENDELA        BEGSR
013100990915     C*
013200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013300080222     C*
013400080222     C* Effettuo lancio TISI95 solo x chiusura
013500080222     C                   CLEAR                   TISI95DS
013600080222     C                   EVAL      I95TLA = 'C'
013700080222     C                   CALL      'TISI95R'
013800080222     C                   PARM                    TISI95DS
013900000613     C*
014000030715     C                   if        %open(titvxlps)
014100000616     C                   except    findett
014200030715     C                   close     titvxlps
014300000613     C                   endif
014400000616     C*
014500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014600011113     C                   Z-ADD     150           LENGH            15 5
014700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014800000616     C                   PARM                    CMD(3)
014900000616     C                   PARM                    LENGH
015000000616     C*
015100010601     C                   ENDSR
015200010601     C***
015300010601
015400010601
015500010601
015600010601
015700000613
015800000613     C*--------------------------------------------------------
015900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016000000613     C*--------------------------------------------------------
016100000621     C     STPR          BEGSR
016200000613     C*
016300030715     C                   if        not %open(titvxlp)
016400030715     C   50              open      titvxlp
016500990915     C                   endif
016600990915     C*
016700030715     C   50              except    riepilogo
016800990915     C*
016900030715     C                   if        %open(titvxlp)
017000030715     C   50              close     titvxlp
017100990914     C                   endif
017200000613     C*
017300000613     C                   ENDSR
017400000613     C***
017500000613
017600010330
017700010601
017800010601
017900010601
018000010330     C*--------------------------------------------------------
018100101018     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
018200010330     C*--------------------------------------------------------
018300010605     C     RWVAB         BEGSR
018400010330     C*
018500010330     C                   if        not %open(tivin00r)
018600010330     C                   open      tivin00r
018700010330     C                   endif
018800101018     C                   if        not %open(edivabwr)
018900101018     C                   open      edivabwr
019000010330     C                   endif
019100010330     C*
019200010604     C                   clear                   §CTROK
019300010604     C                   clear                   §CTRMO
019400010604     C                   clear                   §CTRNO
019500010330     C*
019600030822     C                   do        *HIVAL
019700010330     C*
019800030822     C                   read      tivin00r                               70
019900010618     C*
020000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020100010618     C                   if        vinflg = '1'
020200010618     C                   eval      flgOk = '1'
020300010618     C                   endif
020400010618     C*
020500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020600040510     C                   z-add     *zeros        wLenVINDTA        4 0
020700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021100040510     C                   z-add     1             wFlgVINDTA
021200040510     C                   leave
021300040510     C                   endif
021400040510     C                   enddo
021500040510     C*
021600040218     C                   if        vindta > *blanks AND
021700040510     C                             wFlgVINDTA = 1
021800010330     C                   add       1             rrnum
021900010330     C*
022000010601     C                   if        *in70 = *off and
022100010330     C                             (vinflg = *blanks
022200010330     C                              or vinflg = '0'
022300010330     C                              or vinflg = '2')
022400010330     C*
022500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022600010711     C                   if        vinflg = *blanks or vinflg = '0'
022700010711     C                   clear                   vinmsg
022800010711     C                   endif
022900010601     C*
023000010330     C                   exsr      inzvar
023100100419     C                   exsr      normCSV
023200090403     C*
023300090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
023400090403     C* separatori campi
023500090403     C                   if        CharTXT <> *blanks
023600090403     C                   z-add     *zeros        wLenVINDTA        4 0
023700090403     C                   z-add     *zeros        wFlgVINDTA        1 0
023800090403     C                   setoff                                       80
023900090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
024000090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
024100090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
024200090403     C                   if        not *in80
024300090403     C                   seton                                        80
024400090403     C                   else
024500090403     C                   setoff                                       80
024600090403     C                   endif
024700090403     C                   endif
024800090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
024900090403     C                             and *in80 = *on
025000090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
025100090403     C                   endif
025200090403     C                   enddo
025300090403     C                   endif
025400090403     C*
025500010605     C                   exsr      impvab
025600010601     C*
025700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025800010605     C                   eval      depfil = VABLNP
025900010601     C                   exsr      repfil
026000060331     C                   if        VABFGS = *zeros
026100010601     C                   if        depfil = invfil
026200021025     C                   if        vlrpoi = 999
026300030822     C                   move(P)   invfil        VABFGS
026400021025     C                   else
026500030822     C                   move(P)   vlrpoi        VABFGS
026600021025     C                   endif
026700050113     C                   endif
026800060331     C                   endif
026900010601     C*
027000010601     C                   exsr      PREELA
027100010601     C*
027200010604     C* Ebbene...
027300010604     C*
027400030822     C  N31              add       1             §CTROK            7 0
027500030822     C   32              add       1             §CTRMO            7 0
027600030822     C   31              add       1             §CTRNO            7 0
027700030822     C                   if        wGiro = 2
027800101018     C* VALORIZZO CAMPI RELATIVI AL "CMR"
027900101018     C                   EVAL      VABCMR = wCMR
028000101018     C                   EVAL      VABDCM = DATCOR
028100101018     C                   EVAL      VABDTS = DATCOR
028200101018     C                   EVAL      VABHMS = ORACOR
028300101018     C                   EVAL      VABCNT = 1
028400101018     C*
028500101018     C  N31              write     EDIVAB00
028600030822     C                   endif
028700010604     C*
028800010604     C                   if        *in31 = *off and
028900010604     C                             *in32 = *off
029000010604     C                   eval      vinflg = '1'
029100010604     C                   else
029200010604     C                   eval      recko = vindta
029300070102     C                   if        %open(titvxlps)
029400070102     C                   except    rigadett
029500070102     C                   endif
029600010604     C                   eval      vinflg = '2'
029700010604     C                   endif
029800010601     C*
029900010604     C                   endif
030000010604     C*
030100010330     C                   else
030200010330     C                   eval      vinflg = '1'
030300010330     C                   endif
030400010601     C*
030500010601     C  N70              update    tivin000
030600010330     C*
030700030822     C  N70              enddo
030800010601     C
030900010601     C*
031000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031100010601     C                   if        cntNonEl = *zeros or
031200010601     C                             flgMulti = '0'
031300010330     C* Se non ci sono record con errori ...
031400010601     C                   if        §ctrno = 0 and
031500010604     C                             §ctrmo = 0 and
031600010601     C                             flgStato <> '2'
031700010330     C* ... restituisco esito OK.
031800010330     C                   eval      wrkesito = '0'
031900010330     C                   else
032000010330     C                   if        §ctrok > 0
032100010330     C                   eval      wrkesito = '1'
032200010330     C                   else
032300010615     C                   if        flgOk = '0'
032400010615     C                   eval      wrkesito = '2'
032500010615     C                   else
032600010615     C                   eval      wrkesito = '6'
032700010615     C                   endif
032800010330     C                   endif
032900010330     C                   endif
033000010601     C                   else
033100010601     C                   eval      wrkesito = '9'
033200010601     C                   endif
033300010330     C*
033400010330     C                   if        %open(tivin00r)
033500010330     C                   close     tivin00r
033600010330     C                   endif
033700101018     C                   if        %open(edivabwr)
033800101018     C                   close     edivabwr
033900010330     C                   endif
034000010601     C*
034100010601     C                   if        vlrpoi <> 999
034200010601     C                   eval      invfil = vlrpoi
034300010601     C                   endif
034400010330     C*
034500010330     C                   if        §ctrok > 0
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
035600010601
035700010601
035800010601
035900010330     C*----------------------------------------------------*
036000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036100010330     C*----------------------------------------------------*
036200010330     C     INZVAR        BEGSR
036300010330     C*
036400030822     C* Inizializzo variabili di wrk
036500020204     C                   Z-ADD     *zeros        Num5_0            5 0
036600090403     C                   z-add     *zeros        invfil
036700030822     C*
036800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
036900101018     C                   CLEAR                   EDIVAB00
037000030822     C                   CLEAR                   SkSplitCSV
037100030822     C                   EVAL      i = 1
037200030822     C                   EVAL      posDa = *zeros
037300030822     C                   EVAL      posA  = *zeros
037400030822     C*
037500030822     C* Reimposto i valori di default
037600030822     C                   EXSR      DEFCAM
037700030822     C*
037800010330     C                   ENDSR
037900010330     C*----------------------------------------------------*
038000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
038100010330     C*----------------------------------------------------*
038200010330     C     DEFCAM        BEGSR
038300030822     C*
038400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
038500030715     C* e delimitatore testo.
038600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
038700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
038800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
038900090403     C*
039000090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
039100090403     C                   EVAL      CharSOS = CharNUM
039200030822     C*
039300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
039400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
039500030715     C                   SETON                                        50
039600030715     C                   ELSE
039700030715     C                   SETOFF                                       50
039800030715     C                   ENDIF
039900030822     C*
040000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
040100030822     C                   EVAL      posDaDft = 1
040200030822     C                   EVAL      posADft  = 0
040300030822     C                   EVAL      wGiroDft = 0
040400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
040500030822     C                             posDaDft > 0
040600030822     C*
040700030822     C* Gestisco il 1° giro
040800030822     C                   IF        wGiroDft = 0
040900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
041000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
041100030822     C* Incremento il contatore dei "giri"
041200030822     C                   EVAL      wGiroDft = 1
041300030822     C                   ELSE
041400030822     C                   EVAL      posDaDft = posADft
041500030822     C                   ENDIF
041600030822     C* Eseguo lo scan x trovare la fine del campo corrente
041700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
041800030822     C*
041900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
042000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
042100030822     C                   IF        posDaDft > 0 AND
042200030822     C                             posADft  > 0
042300060331     C* FGS
042400060331     C                   IF        %subst(
042500060331     C                             %subst(vlrppt:posDaDft+1:
042600060331     C                             posADft-posDaDft-1):1:3)
042700060331     C                             = 'FGS'
042800060331     C                   EVAL      PiStr=%trim(%subst(
042900060331     C                             %subst(vlrppt:posDaDft+1:
043000060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043100060331     C                   EXSR      CHKNUM
043200060331     C                   IF        PiInt=*on
043300060331     C                   Z-ADD     PiVal         VABFGS
043400060331     C                   ENDIF
043500060331     C                   ENDIF
043600030822     C* NCL
043700030822     C                   IF        %subst(
043800030822     C                             %subst(vlrppt:posDaDft+1:
043900030822     C                             posADft-posDaDft-1):1:3)
044000030822     C                             = 'NCL'
044100030822     C                   EVAL      PiStr=%trim(%subst(
044200030822     C                             %subst(vlrppt:posDaDft+1:
044300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044400030822     C                   EXSR      CHKNUM
044500030822     C                   IF        PiInt=*on
044600030822     C                   Z-ADD     PiVal         VABNCL
044700030822     C                   ENDIF
044800030822     C                   ENDIF
044900030822     C* CCM
045000030822     C                   IF        %subst(
045100030822     C                             %subst(vlrppt:posDaDft+1:
045200030822     C                             posADft-posDaDft-1):1:3)
045300030822     C                             = 'CCM'
045400030822     C                   EVAL      PiStr=%trim(%subst(
045500030822     C                             %subst(vlrppt:posDaDft+1:
045600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045700030822     C                   EXSR      CHKNUM
045800030822     C                   IF        PiInt=*on
045900030822     C                   Z-ADD     PiVal         VABCCM
046000030822     C                   ENDIF
046100030822     C                   ENDIF
046200030822     C* LNP
046300030822     C                   IF        %subst(
046400030822     C                             %subst(vlrppt:posDaDft+1:
046500030822     C                             posADft-posDaDft-1):1:3)
046600030822     C                             = 'LNP'
046700030822     C                   EVAL      PiStr=%trim(%subst(
046800030822     C                             %subst(vlrppt:posDaDft+1:
046900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047000030822     C                   EXSR      CHKNUM
047100030822     C                   IF        PiInt=*on
047200030822     C                   Z-ADD     PiVal         VABLNP
047300030822     C                   ENDIF
047400030822     C                   ENDIF
047500030822     C* NRS
047600030822     C                   IF        %subst(
047700030822     C                             %subst(vlrppt:posDaDft+1:
047800030822     C                             posADft-posDaDft-1):1:3)
047900030822     C                             = 'NRS'
048000030822     C                   EVAL      PiStr=%trim(%subst(
048100030822     C                             %subst(vlrppt:posDaDft+1:
048200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048300030822     C                   EXSR      CHKNUM
048400030822     C                   IF        PiInt=*on
048500030822     C                   Z-ADD     PiVal         VABNRS
048600030822     C                   ENDIF
048700030822     C                   ENDIF
048800030822     C* CTR
048900030822     C                   IF        %subst(
049000030822     C                             %subst(vlrppt:posDaDft+1:
049100030822     C                             posADft-posDaDft-1):1:3)
049200030822     C                             = 'CTR'
049300030822     C                   EVAL      PiStr=%trim(%subst(
049400030822     C                             %subst(vlrppt:posDaDft+1:
049500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049600030822     C                   EXSR      CHKNUM
049700030822     C                   IF        PiInt=*on
049800030822     C                   Z-ADD     PiVal         VABCTR
049900030822     C                   ENDIF
050000030822     C                   ENDIF
050100030822     C* PKB
050200030822     C                   IF        %subst(
050300030822     C                             %subst(vlrppt:posDaDft+1:
050400030822     C                             posADft-posDaDft-1):1:3)
050500030822     C                             = 'PKB'
050600030822     C                   EVAL      PiStr=%trim(%subst(
050700030822     C                             %subst(vlrppt:posDaDft+1:
050800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050900030822     C                   EXSR      CHKNUM
051000030822     C                   IF        PiNum=*on
051100030822     C                   Z-ADD     PiVal         VABPKB
051200030822     C                   ENDIF
051300030822     C                   ENDIF
051400030822     C* VLB
051500030822     C                   IF        %subst(
051600030822     C                             %subst(vlrppt:posDaDft+1:
051700030822     C                             posADft-posDaDft-1):1:3)
051800030822     C                             = 'VLB'
051900030822     C                   EVAL      PiStr=%trim(%subst(
052000030822     C                             %subst(vlrppt:posDaDft+1:
052100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052200030822     C                   EXSR      CHKNUM
052300030822     C                   IF        PiNum=*on
052400030822     C                   Z-ADD     PiVal         VABVLB
052500030822     C                   ENDIF
052600030822     C                   ENDIF
052700030822     C* QFT
052800030822     C                   IF        %subst(
052900030822     C                             %subst(vlrppt:posDaDft+1:
053000030822     C                             posADft-posDaDft-1):1:3)
053100030822     C                             = 'QFT'
053200030822     C                   EVAL      PiStr=%trim(%subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053500030822     C                   EXSR      CHKNUM
053600030822     C                   IF        PiNum=*on
053700030822     C                   Z-ADD     PiVal         VABQFT
053800030822     C                   ENDIF
053900030822     C                   ENDIF
054000030822     C* CBO
054100030822     C                   IF        %subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):1:3)
054400030822     C                             = 'CBO'
054500030822     C                   EVAL      VABCBO=%trim(%subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054800030822     C                   ENDIF
054900030822     C* TSP
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'TSP'
055400030822     C                   EVAL      VABTSP=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   ENDIF
055800030822     C* VAS
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'VAS'
056300030822     C                   EVAL      VABVAS=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   ENDIF
056700030822     C* VCA
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'VCA'
057200030822     C                   EVAL      VABVCA=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   ENDIF
057600030822     C* GCA
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'GCA'
058100030822     C                   EVAL      VABGCA=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   ENDIF
058500030822     C* CTM
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'CTM'
059000030822     C                   EVAL      VABCTM=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   ENDIF
059400030822     C* FFD
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'FFD'
059900030822     C                   EVAL      VABFFD=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   ENDIF
060300030822     C* VAD
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'VAD'
060800030822     C                   EVAL      VABVAD=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   ENDIF
061200030822     C* GMA
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'GMA'
061700030822     C                   EVAL      VABGMA=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   ENDIF
062100030822     C* GGA
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'GGA'
062600030822     C                   EVAL      VABGGA=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   ENDIF
063000030822     C* GVA
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'GVA'
063500030822     C                   EVAL      VABGVA=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   ENDIF
063900030822     C* TC1
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'TC1'
064400030822     C                   EVAL      VABTC1=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   ENDIF
064800030822     C* TC2
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'TC2'
065300030822     C                   EVAL      VABTC2=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* ...
065800030822     C                   ENDIF
065900030822     C                   ENDDO
066000020204     C*
066100010330     C                   ENDSR
066200010607     C*----------------------------------------------------*
066300101018     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
066400010607     C*----------------------------------------------------*
066500010607     C     IMPVAB        BEGSR
066600010607     C*
066700010607     C                   SETOFF                                       3132
066800030515     C*
066900010607     C                   MOVEL     datcor        VABAAS
067000010607     C                   MOVE      datcor        VABMGS
067100010607     C*
067200030715     C**********
067300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
067400030715     C**********
067500030715     C                   IF        wGiro = *zeros
067600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
067700031201     C     minu:maiu     XLATE     vindta        vindta
067800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
067900040324     C                   DOW       posDa <= %len(%trimr(vindta))
068000030715     C*
068100030715     C* Gestisco il 1° campo
068200030715     C                   IF        i = 1
068300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
068400030715     C                   EVAL      posDa = 1
068500030715     C* Eseguo lo scan x trovare la fine del primo campo
068600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
068700030715     C* A questo punto "estraggo" il campo corrente
068800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
068900030715     C                                                    (posA-posDa))
069000030715     C* X i campi successivi al 1°
069100030715     C                   ELSE
069200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
069300030715     C                   EVAL      posDa = posA + 1
069400030715     C* Eseguo lo scan x trovare la fine del campo corrente
069500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
069600030715     C* Gestisco l'ultimo campo
069700030715     C                   IF        posA = *zeros
069800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
069900030715     C* A questo punto "estraggo" il campo corrente
070000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070100030715     C                                                    (posA-posDa))
070200030715     C                   LEAVE
070300030715     C                   ELSE
070400030715     C* A questo punto "estraggo" il campo corrente
070500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070600030715     C                                                    (posA-posDa))
070700030715     C                   ENDIF
070800030715     C                   ENDIF
070900030715     C* Incremento il contatore d campo
071000030715     C                   EVAL      i = i +1
071100030715     C                   ENDDO
071200030715     C*
071300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
071400030715     C* delimitazione testo
071500030715     C                   EVAL      i = 1
071600030715     C                   DOW       i < %elem(SkSplitFLD)
071700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
071800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
071900030715     C                   EVAL      i = i + 1
072000030715     C                   ENDDO
072100030715     C                   EVAL      wGiro = 1
072200030715     C                   ELSE
072300030715     C                   EVAL      wGiro = 2
072400031204     C**********
072500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
072600031204     C* d separatore campo
072700031204     C**********
072800040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
072900040304     C                   EVAL      vindta = ' ' + vindta
073000040304     C                   ENDIF
073100040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
073200040304     C***                EVAL      vindta = %subst(vindta:2)
073300040304     C***                ENDDO
073400031204     C*
073500031204     C                   Z-ADD     *zeros        lunghInput        4 0
073600040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
073700040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
073800040304     C                   EVAL      vindta = vindta + ' '
073900040304     C                   ENDIF
074000040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
074100040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
074200040304     C***                EVAL      lunghInput = %len(%trim(vindta))
074300040304     C***                ENDDO
074400030715     C**********
074500030715     C* Effettuo lo split del campo dati d input
074600030715     C**********
074700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074800040324     C                   DOW       posDa <= %len(%trimr(vindta))
074900030715     C*
075000030715     C* Gestisco il 1° campo
075100030715     C                   IF        i = 1
075200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075300030715     C                   EVAL      posDa = 1
075400030715     C* Eseguo lo scan x trovare la fine del primo campo
075500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075600030715     C* A questo punto "estraggo" il campo corrente
075700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
075800030715     C                                                    (posA-posDa))
075900030715     C* X i campi successivi al 1°
076000030715     C                   ELSE
076100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076200030715     C                   EVAL      posDa = posA + 1
076300031204     C* Verifico che nn vi sia il campo nn valorizzato
076400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
076500031204     C* Se campo nn valorizzato skippo al prossimo
076600031204     C                   EVAL      SkSplitCSV(i) = *blanks
076700031204     C                   EVAL      posA  = posA + 1
076800031204     C                   ELSE
076900030715     C* Eseguo lo scan x trovare la fine del campo corrente
077000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077100030715     C* Gestisco l'ultimo campo
077200030715     C                   IF        posA = *zeros
077300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
077400030715     C* A questo punto "estraggo" il campo corrente
077500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
077600030715     C                                                    (posA-posDa))
077700030715     C                   LEAVE
077800030715     C                   ELSE
077900030715     C* A questo punto "estraggo" il campo corrente
078000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078100030715     C                                                    (posA-posDa))
078200030715     C                   ENDIF
078300031204     C                   ENDIF
078400030715     C                   ENDIF
078500030715     C* Incremento il contatore d campo
078600030715     C                   EVAL      i = i +1
078700030715     C                   ENDDO
078800030715     C*
078900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079000030715     C* delimitazione testo
079100030715     C                   EVAL      i = 1
079200030715     C                   DOW       i < %elem(SkSplitCSV)
079300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
079400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
079500030715     C                   EVAL      i = i + 1
079600030715     C                   ENDDO
079700030715     C*
079800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
079900030715     C                   EVAL      i = 1
080000030715     C                   DOW       i < %elem(SkSplitFLD)
080100030715     C*
080200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
080300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
080400030715     C                             SkSplitCSV(i) <> *blanks
080500030721     C*** ATB
080600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
080700030721     C                   EVAL      VABATB = SkSplitCSV(i)
080800030721     C                   ENDIF
080900030715     C*** RSD
081000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
081100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
081200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
081300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
081400030715     C* ==
081500030715     C                   ENDIF
081600030721     C*** RD2
081700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
081800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
081900030721     C                   ENDIF
082000030715     C*** IND
082100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
082200030715     C                   EVAL      VABIND = SkSplitCSV(i)
082300030715     C                   ENDIF
082400030715     C*** LOD
082500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
082600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
082700030715     C                   ENDIF
082800030715     C*** PRD
082900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
083000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
083100030715     C                   ENDIF
083200030721     C*** NZD
083300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
083400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
083500030721     C                   ENDIF
083600030715     C*** RMA
083700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
083800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
083900030715     C                   ENDIF
084000030715     C*** NOT
084100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
084200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
084300030715     C                   ENDIF
084400030715     C*** NT2
084500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
084600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
084700030715     C                   ENDIF
084800030715     C*** VCA
084900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
085000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
085100030715     C                   ENDIF
085200030715     C*** VAS
085300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
085400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
085500030715     C                   ENDIF
085600030715     C*** FFD
085700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
085800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
085900030715     C                   ENDIF
086000030715     C*** CBO
086100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
086200030715     C                   EVAL      VABCBO = SkSplitCSV(i)
086300030715     C                   ENDIF
086400030721     C*** GC1
086500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
086600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
086700030721     C                   ENDIF
086800030721     C*** GC2
086900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
087000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
087100030721     C                   ENDIF
087200030721     C*** TSP
087300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
087400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
087500030721     C                   ENDIF
087600030721     C*** NAS
087700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
087800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
087900030721     C                   ENDIF
088000030721     C*** TIC
088100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
088200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
088300030721     C                   ENDIF
088400030721     C*** GCA
088500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
088600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
088700030721     C                   ENDIF
088800030721     C*** XCO
088900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
089000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
089100030721     C                   ENDIF
089200030721     C*** CTM
089300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
089400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
089500030721     C                   ENDIF
089600030721     C*** TCR
089700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
089800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
089900030721     C                   ENDIF
090000030721     C*** CTS
090100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
090200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
090300030721     C                   ENDIF
090400030721     C*** FTM
090500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
090600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
090700030721     C                   ENDIF
090800030721     C*** VAD
090900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
091000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
091100030721     C                   ENDIF
091200030721     C*** GMA
091300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
091400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
091500030721     C                   ENDIF
091600030721     C*** GGA
091700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
091800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
091900030721     C                   ENDIF
092000030721     C*** GVA
092100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
092200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
092300030721     C                   ENDIF
092400030721     C*** TC1
092500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
092600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
092700030721     C                   ENDIF
092800030721     C*** TC2
092900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
093000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
093100030721     C                   ENDIF
093200030721     C*** SCL
093300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
093400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
093500030721     C                   ENDIF
093600030721     C*** RMO
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
093800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030721     C*** NMO
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
094200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030721     C*
094500070705     C* Reperisco quindi i campi numerici...
094600030721     C*** CAD
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
094800070705     C                   EVAL      VABCAD = SkSplitCSV(i)
094900030721     C                   ENDIF
095000030721     C*** RMN
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
095200030721     C                   EVAL      PiStr=SkSplitCSV(i)
095300030721     C                   EXSR      CHKNUM
095400030721     C                   IF        PiInt=*on
095500030721     C                   Z-ADD     PiVal         VABRMN
095600030721     C                   ELSE
095700030721     C                   SETON                                        32
095800030721     C                   EVAL      VABRMN = 1
095900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096000030721     C                             + ' ' + 'VABRMN'
096100030721     C                   ENDIF
096200030721     C                   ENDIF
096300030721     C*** NSP
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
096500030721     C                   EVAL      PiStr=SkSplitCSV(i)
096600030721     C                   EXSR      CHKNUM
096700100517     C                   IF        PiInt=*on
096800030721     C                   Z-ADD     PiVal         VABNSP
096900030721     C                   ELSE
097000030721     C                   SETON                                        32
097100030721     C                   EVAL      VABNSP = *zeros
097200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097300030721     C                             + ' ' + 'VABNSP'
097400030721     C                   ENDIF
097500030721     C                   ENDIF
097600030721     C*** NCL
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
097800030721     C                   EVAL      PiStr=SkSplitCSV(i)
097900030721     C                   EXSR      CHKNUM
098000030721     C                   IF        PiInt=*on
098100030721     C                   Z-ADD     PiVal         VABNCL
098200030721     C                   ELSE
098300030721     C                   SETON                                        32
098400030721     C                   EVAL      VABNCL = *zeros
098500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098600030721     C                             + ' ' + 'VABNCL'
098700030721     C                   ENDIF
098800030721     C                   ENDIF
098900030721     C*** PKB
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
099100030721     C                   EVAL      PiStr=SkSplitCSV(i)
099200030721     C                   EXSR      CHKNUM
099300030721     C                   IF        PiNum=*on
099400030721     C                   Z-ADD     PiVal         VABPKB
099500030721     C                   ELSE
099600030721     C                   SETON                                        32
099700030721     C                   EVAL      VABPKB = *zeros
099800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099900030721     C                             + ' ' + 'VABPKB'
100000030721     C                   ENDIF
100100030721     C                   ENDIF
100200030721     C*** CAS
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
100400030721     C                   EVAL      PiStr=SkSplitCSV(i)
100500030721     C                   EXSR      CHKNUM
100600030721     C                   IF        PiNum=*on
100700030721     C                   Z-ADD     PiVal         VABCAS
100800030721     C                   ELSE
100900030721     C                   SETON                                        32
101000030721     C                   EVAL      VABCAS = *zeros
101100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101200030721     C                             + ' ' + 'VABCAS'
101300030721     C                   ENDIF
101400030721     C                   ENDIF
101500030721     C*** CCM
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
101700030721     C                   EVAL      PiStr=SkSplitCSV(i)
101800030721     C                   EXSR      CHKNUM
101900030721     C                   IF        PiInt=*on      AND
102000030721     C                             PiVal<=9999999 AND
102100030721     C                             PiVal>*zeros
102200030721     C                   Z-ADD     PiVal         VABCCM
102300030721     C                   ELSE
102400030721     C                   SETON                                        32
102500030721     C                   EVAL      VABCCM = *zeros
102600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102700030721     C                             + ' ' + 'VABCCM'
102800030721     C                   ENDIF
102900030721     C                   ENDIF
103000030721     C*** LNP
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
103200030721     C                   EVAL      PiStr=SkSplitCSV(i)
103300030721     C                   EXSR      CHKNUM
103400030721     C                   IF        PiInt=*on    AND
103500030721     C                             PiVal<=999   AND
103600030721     C                             PiVal>*zeros
103700030721     C                   Z-ADD     PiVal         VABLNP
103800030721     C                   ELSE
103900030721     C                   SETON                                        32
104000030721     C                   EVAL      VABLNP = *zeros
104100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104200030721     C                             + ' ' + 'VABLNP'
104300030721     C                   ENDIF
104400030721     C                   ENDIF
104500030721     C*** NRS
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
104700030721     C                   EVAL      PiStr=SkSplitCSV(i)
104800030721     C                   EXSR      CHKNUM
104900030721     C                   IF        PiInt=*on AND
105000030721     C                             PiVal<=99
105100030721     C                   Z-ADD     PiVal         VABNRS
105200030721     C                   ELSE
105300030721     C                   SETON                                        32
105400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105500030721     C                             + ' ' + 'VABNRS'
105600030721     C                   ENDIF
105700030721     C                   ENDIF
105800030721     C*** LNA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
106000030721     C                   EVAL      PiStr=SkSplitCSV(i)
106100030721     C                   EXSR      CHKNUM
106200030721     C                   IF        PiInt=*on    AND
106300030721     C                             PiVal<=999   AND
106400030721     C                             PiVal>*zeros
106500030721     C                   Z-ADD     PiVal         VABLNA
106600030721     C                   ELSE
106700030721     C                   SETON                                        32
106800030721     C                   EVAL      VABLNA = *zeros
106900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107000030721     C                             + ' ' + 'VABLNA'
107100030721     C                   ENDIF
107200030721     C                   ENDIF
107300030721     C*** CTR
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
107500030721     C                   EVAL      PiStr=SkSplitCSV(i)
107600030721     C                   EXSR      CHKNUM
107700030721     C                   IF        PiInt=*on  AND
107800030721     C                             PiVal<=999
107900030721     C                   Z-ADD     PiVal         VABCTR
108000030721     C                   ELSE
108100030721     C                   SETON                                        32
108200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108300030721     C                             + ' ' + 'VABCTR'
108400030721     C                   ENDIF
108500030721     C                   ENDIF
108600030721     C*** IAS
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
108800030721     C                   EVAL      PiStr=SkSplitCSV(i)
108900030721     C                   EXSR      CHKNUM
109000030721     C                   IF        PiNum=*on
109100030721     C                   Z-ADD     PiVal         VABIAS
109200030721     C                   ELSE
109300030721     C                   SETON                                        32
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500030721     C                             + ' ' + 'VABIAS'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** VLB
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiNum=*on
110300030721     C                   Z-ADD     PiVal         VABVLB
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110700030721     C                             + ' ' + 'VABVLB'
110800030721     C                   ENDIF
110900030721     C                   ENDIF
111000030721     C*** QFT
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiNum=*on
111500030721     C                   Z-ADD     PiVal         VABQFT
111600030721     C                   ELSE
111700030721     C                   SETON                                        32
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900030721     C                             + ' ' + 'VABQFT'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** NCD
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on      AND
112700030721     C                             PiVal<=9999999
112800030721     C                   Z-ADD     PiVal         VABNCD
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABNCD'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** NCA
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on      AND
114000030721     C                             PiVal<=9999999
114100030721     C                   Z-ADD     PiVal         VABNCA
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500030721     C                             + ' ' + 'VABNCA'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** ZNC
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiInt=*on AND
115300030721     C                             PiVal<=99
115400030721     C                   Z-ADD     PiVal         VABZNC
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABZNC'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** DCR
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on       AND
116600030721     C                             PiVal<=99999999
116700030721     C                   Z-ADD     PiVal         VABDCR
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100030721     C                             + ' ' + 'VABDCR'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** HCR
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on   AND
117900030721     C                             PiVal<=9999
118000030721     C                   Z-ADD     PiVal         VABHCR
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABHCR'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** VMD
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiNum=*on
119200030721     C                   Z-ADD     PiVal         VABVMD
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABVMD'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** ANT
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on        AND
120400030721     C                             PiVal<=999999999
120500030721     C                   Z-ADD     PiVal         VABANT
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABANT'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** CMO
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on
121700030721     C                   Z-ADD     PiVal         Num5_0
121800030721     C                   MOVEL(p)  Num5_0        VABCMO
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      VABCMO = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABCMO'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030715     C*
122700030715     C                   ENDIF
122800030715     C                   EVAL      i = i + 1
122900030715     C                   ENDDO
123000070705     C*
123100070705     C* Considerazioni particolari
123200070705     C*** NZD
123300070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
123400070705     C                   EVAL      VABNZD = *blanks
123500070705     C                   ENDIF
123600070705     C*** CAD
123700070705     C                   IF        VABNZD = *blanks
123800070705     C                   EVAL      PiStr=VABCAD
123900070705     C                   EXSR      CHKNUM
124000070705     C                   IF        PiInt=*on
124100070705     C                   Z-ADD     PiVal         Num5_0
124200070705     C                   MOVEL(p)  Num5_0        VABCAD
124300070705     C                   ELSE
124400070705     C                   SETON                                        32
124500070705     C                   EVAL      VABCAD = *zeros
124600070705     C                   EVAL      vinmsg = %trimr(vinmsg)
124700070705     C                             + ' ' + 'VABCAD'
124800070705     C                   ENDIF
124900070705     C                   ENDIF
125000080222     C*
125100080222     C* Se provincia nn valorizzata la reperisco
125200080222     C* tramite TISI95R a seconda dei dati d instradamento presenti
125300080222     C                   IF        VABPRD = *blanks
125400080222     C                   CLEAR                   TISI95DS
125500080222     C                   EVAL      I95TCN = '3'
125600080222     C                   Z-ADD     datcor        I95DAT
125700080222     C                   EVAL      I95NAR = VABNZD
125800080222     C                   EVAL      I95CAP = VABCAD
125900080222     C                   EVAL      I95LOC = VABLOD
126000080222     C                   CALL      'TISI95R'
126100080222     C                   PARM                    TISI95DS
126200080222     C                   EVAL      VABPRD = O95PRV
126300080222     C                   ENDIF
126400020204     C*
126500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
126600020204     C                   EXSR      CHKIMPDIV
126700030715     C                   ENDIF
126800020204     C*
126900010607     C                   ENDSR
127000010607     C*----------------------------------------------------*
127100010601
127200020204
127300020204     C*----------------------------------------------------*
127400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
127500020204     C*----------------------------------------------------*
127600020204     C     CHKIMPDIV     BEGSR
127700060524     C*
127800060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
127900060524     C                   IF        vabCAS > *zeros
128000060524     C                   IF        vabCBO = '1'
128100060524     C                   EVAL      vabCBO = '4'
128200060524     C                   ENDIF
128300060524     C                   IF        vabCBO = '2'
128400060524     C                   EVAL      vabCBO = '6'
128500060524     C                   ENDIF
128600060524     C                   ENDIF
128700020204     C*
128800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
128900020204     C                   Z-ADD     *zeros        wrkDec            9 9
129000020204     C*
129100020204     C* Come prima cosa effettuo considerazioni sulla divisa
129200020204     C                   IF        vabIAS > *zeros
129300020204     C                   IF        vabVAS <> 'EUR'
129400020204     C                   EVAL      vabVAS =  'ITL'
129500020204     C                   ENDIF
129600020204     C                   ENDIF
129700020204     C*
129800020204     C                   IF        vabCAS > *zeros
129900020204     C                   IF        vabVCA <> 'EUR'
130000020204     C                   EVAL      vabVCA =  'ITL'
130100020204     C                   ENDIF
130200020204     C                   ENDIF
130300020204     C*
130400020204     C                   IF        vabVMD > *zeros
130500020204     C                   IF        vabVAD <> 'EUR'
130600020204     C                   EVAL      vabVAD =  'ITL'
130700020204     C                   ENDIF
130800020204     C                   ENDIF
130900020204     C*
131000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
131100020204     C                   Z-ADD     vabIAS        wrkDec
131200020204     C                   IF        wrkDec > *zeros
131300020204     C                   IF        vabVAS = 'ITL'
131400020204     C                   EVAL      vabIAS = *zeros
131500020204     C                   ENDIF
131600020204     C                   ENDIF
131700020204     C*
131800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
131900020204     C                   Z-ADD     vabCAS        wrkDec
132000020204     C                   IF        wrkDec > *zeros
132100020204     C                   IF        vabVCA = 'ITL'
132200020204     C                   EVAL      vabCAS = *zeros
132300020204     C                   ENDIF
132400020204     C                   ENDIF
132500020204     C*
132600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
132700020204     C                   Z-ADD     vabVMD        wrkDec
132800020204     C                   IF        wrkDec > *zeros
132900020204     C                   IF        vabVAD = 'ITL'
133000020204     C                   EVAL      vabVMD = *zeros
133100020204     C                   ENDIF
133200020204     C                   ENDIF
133300020204     C*
133400020204     C                   ENDSR
133500020204     C***
133600020204
133700010330
133800010330
133900010330     C*----------------------------------------------------*
134000010330     C*  CONTROLLO NUMERICITA' CAMPI
134100010330     C*----------------------------------------------------*
134200010330     C     CHKNUM        BEGSR
134300010330     C*
134400010606     C                   IF        PiDecChr = *blanks
134500030715     C                   EVAL      PiDecChr = CharNUM
134600010606     C                   ENDIF
134700010606     C*
134800010606     C                   CALL(e)   'ISNUMERIC'
134900010330     C                   PARM                    PiStr            30
135000010606     C                   PARM                    PiDecChr          1
135100010330     C                   PARM      *ZEROS        PiVal            30 9
135200010330     C                   PARM      '0'           PiInt             1
135300010330     C                   PARM      '0'           PiNum             1
135400010330     C                   IF        %error
135500010606     C                   EVAL      PiNum=*off
135600010330     C                   ENDIF
135700010330     C*
135800010330     C                   ENDSR
135900010330     C***
136000010330
136100010601
136200010601
136300010601
136400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
136500010601     C     repfil        BEGSR
136600010601     C*
136700010601     C                   if        invfil = *zeros and
136800010601     C                             depfil > *zeros and
136900010629     C                             (vinflg = *blanks or
137000010629     C                              vinflg = *zeros)
137100010601     C
137200010601     C                   eval      invfil = depfil
137300010601     C                   endif
137400010601     C*
137500010601     C                   if        depfil <> invfil and
137600010601     C                             invfil > *zeros
137700010601     C                   eval      flgMulti = '1'
137800010601     C                   if        vinflg = *blanks
137900010601     C                   add       1             cntNonEl
138000010601     C                   endif
138100010601     C                   endif
138200010601     C*
138300010601     C                   if        vinflg = '2'
138400010601     C                   eval      flgStato = '2'
138500010601     C                   endif
138600010601     C*
138700010601     C                   ENDSR
138800010601     C***
138900010601
139000010601
139100010601
139200010330
139300010330
139400010330
139500990920      /TITLE Invio dei dati al punto operativo.
139600000613     C     invio         BEGSR
139700990920     C*
139800990920     C                   reset                   dscmz
139900010601     C                   move      invfil        cmzdst
140000990920     C                   eval      cmzfld = vlrfou
140100990920     C                   eval      cmzmbd = vlrhdl
140200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
140300000710     C                   if        prmfir = *blanks
140400101018     C                   eval      cmzfla = 'EDIVAB0F'
140500101018     C                   eval      cmzmba = 'EDIVAB0F'
140600000710     C                   else
140700000710     C                   eval      cmzfla = prmfir
140800000710     C                   eval      cmzmba = prmfir
140900000710     C                   endif
141000990920     C                   eval      cmznrr = *zeros
141100990920     C                   move      §ctrok        cmznrr
141200021018     C                   eval      cmzlba = vlrfl1
141300990920     C                   call(e)   'TIS711C'
141400990920     C                   parm                    dscmz
141500990921     C                   parm      *blanks       esito
141600990920     C                   if        %error
141700990920     C                             or cmzerr = '1'
141800990921     C                             or esito  = '1'
141900000710     C                   eval      wrkesito = '3'
142000990920     C                   endif
142100990920     C*
142200000613     C                   ENDSR
142300990910
142400010601
142500010601
142600010601
142700010601
142800010601      /TITLE Invio dei dati al punto operativo.
142900010601     C     opeini        BEGSR
143000010601     C*
143100010601     C* Inizializzo flag e contatori operativi
143200010601     C                   movel     '0'           flgGiro           1
143300010601     C                   movel     '0'           flgMulti          1
143400010601     C                   movel     '1'           flgStato          1
143500010615     C                   movel     '0'           flgOk             1
143600010601     C                   z-add     *zeros        cntNonEl         10 0
143700010601     C                   z-add     *zeros        depfil            3 0
143800010601     C                   z-add     *zeros        invfil            3 0
143900010601     C*
144000010601     C                   ENDSR
144100010601     C***
144200050421
144300050421
144400050421
144500050421     C     *pssr         BEGSR
144600050421     C*
144700050421     C                   eval      wrkesito = '2'
144800050421     C*
144900050421     C                   ENDSR     '*CANCL'
145000050421     C***
145100100419
145200100419
145300100419
145400100419     C     normCSV       BEGSR
145500100419     C*
145600100419     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
145700100419     C* separatori campi
145800100419     C                   if        CharTXT <> *blanks
145900100419     C                   z-add     *zeros        wLenVINDTA        4 0
146000100419     C                   z-add     *zeros        wFlgVINDTA        1 0
146100100419     C                   setoff                                       80
146200100419     C                   dow       wLenVINDTA < %len(%trim(vindta))
146300100419     C                   eval      wLenVINDTA = wLenVINDTA + 1
146400100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
146500100419     C                   if        not *in80
146600100419     C                   seton                                        80
146700100419     C                   else
146800100419     C                   setoff                                       80
146900100419     C                   endif
147000100419     C                   endif
147100100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
147200100419     C                             and *in80 = *on
147300100419     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
147400100419     C                   endif
147500100419     C                   enddo
147600100419     C                   endif
147700100419     C*
147800100419     C                   ENDSR
147900100419     C***
148000010601
148100010330
148200010330
148300000613     C     *inzsr        BEGSR
148400990910     C*
148500990910     C     *entry        plist
148600990920     C                   parm                    tivlrds
148700990921     C                   parm      wrkesito      esito
148800000724     C                   parm                    prmlit
148900000710     C                   parm                    prmfir
149000010330     C*
149100010330     C* CALCOLA LA DATA CORRENTE
149200010330     C                   time                    wn14             14 0
149300101018     C                   movel     wn14          oracor            6 0          *ORA
149400121109     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
149500121109     C                   eval      datcor = %dec(%date() : *ISO)
149600121109     C*
149700121109     C* Determino il CMR
149800121109     C                   movel     *blanks       wCMR             35
149900130220     C                   eval      wCMR = %char(datcor)
150000000613     C*
150100000613     C                   ENDSR
150200000613     C***
150300990908
150400030715     Otitvxlp   E            riepilogo         2
150500990915     O                                              'Upload via Internet'
150600990915     O                                           +1 'Traduzione TIVIN00R -'
150700101018     O                                           +1 'EDIVABWR'
150800990915     O                                           +1 'Testate bolle'
150900990915     O          E            riepilogo   2
151000990915     O                       wrkdata
151100990915     O                       wrkora              +1
151200990915     O                       procname            +1
151300990915     O          E            riepilogo   2
151400990915     O                                              'Cliente..................:'
151500010605     O                       VABCCM        z     +1
151600990915     O          E            riepilogo   2
151700990920     O                                              'Riferimento Strategi.....:'
151800990920     O                       vlrhdl              +1
151900990915     O          E            riepilogo   2
152000990915     O                                              'Giusti...................:'
152100971022     O                       §CTROK        2   +  1
152200990915     O          E            riepilogo   2
152300990915     O                                              'Sbagliati e corretti.....:'
152400971022     O                       §CTRMO        2   +  1
152500990915     O          E            riepilogo   2
152600990915     O                                              'Sbagliati e scartati.....:'
152700971022     O                       §CTRNO        2   +  1
152800000613
152900030715     Otitvxlps  E            testdett          2
153000000613     O                                              'Upload via Internet'
153100000613     O                                           +1 'Traduzione TIVIN00R -'
153200101018     O                                           +1 'EDIVABWR'
153300000613     O                                           +1 'Testate bolle'
153400000616     O          E            testdett    3
153500000613     O                                           +2 'N° rec'
153600000613     O                                           +3 'Anteprima contenuto'
153700000616     O          E            rigadett    2
153800000613     O                       rrnum               +2
153900000621     O                       recko               +3
154000000616     O          E            findett     2
154100000613     O                       wrkdata
154200000613     O                       wrkora              +1
154300000613     O                       procname            +1
154400000616     O          E            findett     2
154500000613     O                                              'Cliente..................:'
154600010605     O                       VABCCM        z     +1
154700000616     O          E            findett     2
154800000613     O                                              'Riferimento Strategi.....:'
154900000613     O                       vlrhdl              +1
155000000616     O          E            findett     2
155100000613     O                                              'Giusti...................:'
155200000613     O                       §CTROK        2   +  1
155300000616     O          E            findett     2
155400000613     O                                              'Sbagliati e corretti.....:'
155500000613     O                       §CTRMO        2   +  1
155600000616     O          E            findett     2
155700000613     O                                              'Sbagliati e scartati.....:'
155800000613     O                       §CTRNO        2   +  1
155900000613** CMD - COMANDI CL
156000030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
156100030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
156200030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
156300010606
156400000613
