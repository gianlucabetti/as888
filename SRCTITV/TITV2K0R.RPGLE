000100101018      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200090323     H dftactgrp(*yes)
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
005300010330     D*------------------
005400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500010330     D*------------------
005600010330     D WLBDA8          DS                  INZ
005700010330     D  G08DAT                 1      8  0
005800010330     D  G08INV                 9     16  0
005900010330     D  G08ERR                17     17
006000010330     D  G08TGI                18     22  0
006100031201     D*------------------
006200031201     D* Costanti
006300031201     D*------------------
006400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006600010330
006700010330
006800990915     C                   time                    wrkdata
006900990915     C                   time                    wrkora
007000000913     C                   reset                   rrnum
007100990921     C                   reset                   esito
007200990921     C                   reset                   wrkesito
007300010601     C*
007400010601     C                   exsr      opeini
007500010605     C                   exsr      rwvab
007600010601     C*
007700010601     C                   seton                                        lr
007800010601
007900010601
008000010601
008100010601
008200010601     C*--------------------------------------------------------
008300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008400010601     C*--------------------------------------------------------
008500010601     C     PREELA        BEGSR
008600010601     C*
008700010601     C* SE OCCORRE SPEDIRE IN FILIALE
008800010601     C                   if        invfil <> *zeros and
008900010601     C                             flgGiro = '0'
009000010601     C*
009100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009200010601     C                   eval      flgGiro = '1'
009300000724     C*
009400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009600010601     C     invfil        chain     azorg01l
009700000724     C                   if        %found
009800000724     C                   movel(p)  CMD(1)        depcmd
009900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010000000724     C*
010100000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010200011113     C                   Z-ADD     150           LENGH            15 5
010300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010400000724     C                   PARM                    depcmd
010500000724     C                   PARM                    LENGH
010600000724     C*
010700000724     C                   endif
010800000616     C*
010900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011000000616     C                   movel(p)  CMD(2)        depcmd
011100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011200000616     C*
011300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000616     C                   PARM                    depcmd
011700000616     C                   PARM                    LENGH
011800000613     C*
011900030715     C                   if        not %open(titvxlps)
012000030715     C                   open      titvxlps
012100030715     C                   except    testdett
012200000613     C                   endif
012300010601     C*
012400010601     C                   endif
012500010601     C*
012600010601     C                   ENDSR
012700010601     C***
012800010601
012900010601
013000010601
013100010601
013200010601
013300010601
013400010601
013500010601     C*--------------------------------------------------------
013600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013700010601     C*--------------------------------------------------------
013800010601     C     ENDELA        BEGSR
013900990915     C*
014000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014100080222     C*
014200080222     C* Effettuo lancio TISI95 solo x chiusura
014300080222     C                   CLEAR                   TISI95DS
014400080222     C                   EVAL      I95TLA = 'C'
014500080222     C                   CALL      'TISI95R'
014600080222     C                   PARM                    TISI95DS
014700000613     C*
014800030715     C                   if        %open(titvxlps)
014900000616     C                   except    findett
015000030715     C                   close     titvxlps
015100000613     C                   endif
015200000616     C*
015300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015400011113     C                   Z-ADD     150           LENGH            15 5
015500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015600000616     C                   PARM                    CMD(3)
015700000616     C                   PARM                    LENGH
015800000616     C*
015900010601     C                   ENDSR
016000010601     C***
016100010601
016200010601
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
018300000613
018400010330
018500010601
018600010601
018700010601
018800010330     C*--------------------------------------------------------
018900101018     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
019000010330     C*--------------------------------------------------------
019100010605     C     RWVAB         BEGSR
019200010330     C*
019300010330     C                   if        not %open(tivin00r)
019400010330     C                   open      tivin00r
019500010330     C                   endif
019600101018     C                   if        not %open(edivabwr)
019700101018     C                   open      edivabwr
019800010330     C                   endif
019900010330     C*
020000010604     C                   clear                   §CTROK
020100010604     C                   clear                   §CTRMO
020200010604     C                   clear                   §CTRNO
020300010330     C*
020400030822     C                   do        *HIVAL
020500010330     C*
020600030822     C                   read      tivin00r                               70
020700010618     C*
020800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020900010618     C                   if        vinflg = '1'
021000010618     C                   eval      flgOk = '1'
021100010618     C                   endif
021200010618     C*
021300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021400040510     C                   z-add     *zeros        wLenVINDTA        4 0
021500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021900040510     C                   z-add     1             wFlgVINDTA
022000040510     C                   leave
022100040510     C                   endif
022200040510     C                   enddo
022300040510     C*
022400040218     C                   if        vindta > *blanks AND
022500040510     C                             wFlgVINDTA = 1
022600010330     C                   add       1             rrnum
022700010330     C*
022800010601     C                   if        *in70 = *off and
022900010330     C                             (vinflg = *blanks
023000010330     C                              or vinflg = '0'
023100010330     C                              or vinflg = '2')
023200010330     C*
023300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023400010711     C                   if        vinflg = *blanks or vinflg = '0'
023500010711     C                   clear                   vinmsg
023600010711     C                   endif
023700010601     C*
023800010330     C                   exsr      inzvar
023900100419     C                   exsr      normCSV
024000090403     C*
024100090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
024200090403     C* separatori campi
024300090403     C                   if        CharTXT <> *blanks
024400090403     C                   z-add     *zeros        wLenVINDTA        4 0
024500090403     C                   z-add     *zeros        wFlgVINDTA        1 0
024600090403     C                   setoff                                       80
024700090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
024800090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
024900090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
025000090403     C                   if        not *in80
025100090403     C                   seton                                        80
025200090403     C                   else
025300090403     C                   setoff                                       80
025400090403     C                   endif
025500090403     C                   endif
025600090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
025700090403     C                             and *in80 = *on
025800090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
025900090403     C                   endif
026000090403     C                   enddo
026100090403     C                   endif
026200090403     C*
026300010605     C                   exsr      impvab
026400010601     C*
026500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026600010605     C                   eval      depfil = VABLNP
026700010601     C                   exsr      repfil
026800060331     C                   if        VABFGS = *zeros
026900010601     C                   if        depfil = invfil
027000021025     C                   if        vlrpoi = 999
027100030822     C                   move(P)   invfil        VABFGS
027200021025     C                   else
027300030822     C                   move(P)   vlrpoi        VABFGS
027400021025     C                   endif
027500050113     C                   endif
027600060331     C                   endif
027700010601     C*
027800010601     C                   exsr      PREELA
027900010601     C*
028000010604     C* Ebbene...
028100010604     C*
028200030822     C  N31              add       1             §CTROK            7 0
028300030822     C   32              add       1             §CTRMO            7 0
028400030822     C   31              add       1             §CTRNO            7 0
028500030822     C                   if        wGiro = 2
028600101018     C* VALORIZZO CAMPI RELATIVI AL "CMR"
028700101018     C                   EVAL      VABCMR = wCMR
028800101018     C                   EVAL      VABDCM = DATCOR
028900101018     C                   EVAL      VABDTS = DATCOR
029000101018     C                   EVAL      VABHMS = ORACOR
029100101018     C                   EVAL      VABCNT = 1
029200101018     C*
029300120309     C* Forzatura "speciale"
029400120309     C                   select
029500120309     C                   when      vabCBO = '4'
029600120309     C                   eval      vabCBO = '1'
029700120309     C                   when      vabCBO = '6'
029800120309     C                   eval      vabCBO = '2'
029900120309     C                   endsl
030000120309     C*
030100101018     C  N31              write     EDIVAB00
030200030822     C                   endif
030300010604     C*
030400010604     C                   if        *in31 = *off and
030500010604     C                             *in32 = *off
030600010604     C                   eval      vinflg = '1'
030700010604     C                   else
030800010604     C                   eval      recko = vindta
030900070102     C                   if        %open(titvxlps)
031000070102     C                   except    rigadett
031100070102     C                   endif
031200010604     C                   eval      vinflg = '2'
031300010604     C                   endif
031400010601     C*
031500010604     C                   endif
031600010604     C*
031700010330     C                   else
031800010330     C                   eval      vinflg = '1'
031900010330     C                   endif
032000010601     C*
032100010601     C  N70              update    tivin000
032200010330     C*
032300030822     C  N70              enddo
032400010601     C
032500010601     C*
032600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
032700010601     C                   if        cntNonEl = *zeros or
032800010601     C                             flgMulti = '0'
032900010330     C* Se non ci sono record con errori ...
033000010601     C                   if        §ctrno = 0 and
033100010604     C                             §ctrmo = 0 and
033200010601     C                             flgStato <> '2'
033300010330     C* ... restituisco esito OK.
033400010330     C                   eval      wrkesito = '0'
033500010330     C                   else
033600010330     C                   if        §ctrok > 0
033700010330     C                   eval      wrkesito = '1'
033800010330     C                   else
033900010615     C                   if        flgOk = '0'
034000010615     C                   eval      wrkesito = '2'
034100010615     C                   else
034200010615     C                   eval      wrkesito = '6'
034300010615     C                   endif
034400010330     C                   endif
034500010330     C                   endif
034600010601     C                   else
034700010601     C                   eval      wrkesito = '9'
034800010601     C                   endif
034900010330     C*
035000010330     C                   if        %open(tivin00r)
035100010330     C                   close     tivin00r
035200010330     C                   endif
035300101018     C                   if        %open(edivabwr)
035400101018     C                   close     edivabwr
035500010330     C                   endif
035600010601     C*
035700010601     C                   if        vlrpoi <> 999
035800010601     C                   eval      invfil = vlrpoi
035900010601     C                   endif
036000010330     C*
036100010330     C                   if        §ctrok > 0
036200010601     C                             and invfil > *zeros
036300010330     C                   exsr      invio
036400010330     C                   endif
036500010601     C*
036600010618     C                   if        flgGiro = '1'
036700010601     C                   exsr      endela
036800010618     C                   endif
036900010330     C*
037000010330     C                   ENDSR
037100010330     C***
037200010601
037300010601
037400010601
037500010330     C*----------------------------------------------------*
037600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
037700010330     C*----------------------------------------------------*
037800010330     C     INZVAR        BEGSR
037900010330     C*
038000030822     C* Inizializzo variabili di wrk
038100020204     C                   Z-ADD     *zeros        Num5_0            5 0
038200090403     C                   z-add     *zeros        invfil
038300030822     C*
038400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
038500101018     C                   CLEAR                   EDIVAB00
038600030822     C                   CLEAR                   SkSplitCSV
038700030822     C                   EVAL      i = 1
038800030822     C                   EVAL      posDa = *zeros
038900030822     C                   EVAL      posA  = *zeros
039000030822     C*
039100030822     C* Reimposto i valori di default
039200030822     C                   EXSR      DEFCAM
039300030822     C*
039400010330     C                   ENDSR
039500010330     C*----------------------------------------------------*
039600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
039700010330     C*----------------------------------------------------*
039800010330     C     DEFCAM        BEGSR
039900030822     C*
040000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
040100030715     C* e delimitatore testo.
040200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
040300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
040400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
040500090403     C*
040600090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
040700090403     C                   EVAL      CharSOS = CharNUM
040800030822     C*
040900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
041000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
041100030715     C                   SETON                                        50
041200030715     C                   ELSE
041300030715     C                   SETOFF                                       50
041400030715     C                   ENDIF
041500030822     C*
041600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
041700030822     C                   EVAL      posDaDft = 1
041800030822     C                   EVAL      posADft  = 0
041900030822     C                   EVAL      wGiroDft = 0
042000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
042100030822     C                             posDaDft > 0
042200030822     C*
042300030822     C* Gestisco il 1° giro
042400030822     C                   IF        wGiroDft = 0
042500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
042600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
042700030822     C* Incremento il contatore dei "giri"
042800030822     C                   EVAL      wGiroDft = 1
042900030822     C                   ELSE
043000030822     C                   EVAL      posDaDft = posADft
043100030822     C                   ENDIF
043200030822     C* Eseguo lo scan x trovare la fine del campo corrente
043300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
043400030822     C*
043500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
043600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
043700030822     C                   IF        posDaDft > 0 AND
043800030822     C                             posADft  > 0
043900060331     C* FGS
044000060331     C                   IF        %subst(
044100060331     C                             %subst(vlrppt:posDaDft+1:
044200060331     C                             posADft-posDaDft-1):1:3)
044300060331     C                             = 'FGS'
044400060331     C                   EVAL      PiStr=%trim(%subst(
044500060331     C                             %subst(vlrppt:posDaDft+1:
044600060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044700060331     C                   EXSR      CHKNUM
044800060331     C                   IF        PiInt=*on
044900060331     C                   Z-ADD     PiVal         VABFGS
045000060331     C                   ENDIF
045100060331     C                   ENDIF
045200030822     C* NCL
045300030822     C                   IF        %subst(
045400030822     C                             %subst(vlrppt:posDaDft+1:
045500030822     C                             posADft-posDaDft-1):1:3)
045600030822     C                             = 'NCL'
045700030822     C                   EVAL      PiStr=%trim(%subst(
045800030822     C                             %subst(vlrppt:posDaDft+1:
045900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046000030822     C                   EXSR      CHKNUM
046100030822     C                   IF        PiInt=*on
046200030822     C                   Z-ADD     PiVal         VABNCL
046300030822     C                   ENDIF
046400030822     C                   ENDIF
046500030822     C* CCM
046600030822     C                   IF        %subst(
046700030822     C                             %subst(vlrppt:posDaDft+1:
046800030822     C                             posADft-posDaDft-1):1:3)
046900030822     C                             = 'CCM'
047000030822     C                   EVAL      PiStr=%trim(%subst(
047100030822     C                             %subst(vlrppt:posDaDft+1:
047200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047300030822     C                   EXSR      CHKNUM
047400030822     C                   IF        PiInt=*on
047500030822     C                   Z-ADD     PiVal         VABCCM
047600030822     C                   ENDIF
047700030822     C                   ENDIF
047800030822     C* LNP
047900030822     C                   IF        %subst(
048000030822     C                             %subst(vlrppt:posDaDft+1:
048100030822     C                             posADft-posDaDft-1):1:3)
048200030822     C                             = 'LNP'
048300030822     C                   EVAL      PiStr=%trim(%subst(
048400030822     C                             %subst(vlrppt:posDaDft+1:
048500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048600030822     C                   EXSR      CHKNUM
048700030822     C                   IF        PiInt=*on
048800030822     C                   Z-ADD     PiVal         VABLNP
048900030822     C                   ENDIF
049000030822     C                   ENDIF
049100030822     C* NRS
049200030822     C                   IF        %subst(
049300030822     C                             %subst(vlrppt:posDaDft+1:
049400030822     C                             posADft-posDaDft-1):1:3)
049500030822     C                             = 'NRS'
049600030822     C                   EVAL      PiStr=%trim(%subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049900030822     C                   EXSR      CHKNUM
050000030822     C                   IF        PiInt=*on
050100030822     C                   Z-ADD     PiVal         VABNRS
050200030822     C                   ENDIF
050300030822     C                   ENDIF
050400030822     C* CTR
050500030822     C                   IF        %subst(
050600030822     C                             %subst(vlrppt:posDaDft+1:
050700030822     C                             posADft-posDaDft-1):1:3)
050800030822     C                             = 'CTR'
050900030822     C                   EVAL      PiStr=%trim(%subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051200030822     C                   EXSR      CHKNUM
051300030822     C                   IF        PiInt=*on
051400030822     C                   Z-ADD     PiVal         VABCTR
051500030822     C                   ENDIF
051600030822     C                   ENDIF
051700030822     C* PKB
051800030822     C                   IF        %subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):1:3)
052100030822     C                             = 'PKB'
052200030822     C                   EVAL      PiStr=%trim(%subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052500030822     C                   EXSR      CHKNUM
052600030822     C                   IF        PiNum=*on
052700030822     C                   Z-ADD     PiVal         VABPKB
052800030822     C                   ENDIF
052900030822     C                   ENDIF
053000030822     C* VLB
053100030822     C                   IF        %subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):1:3)
053400030822     C                             = 'VLB'
053500030822     C                   EVAL      PiStr=%trim(%subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053800030822     C                   EXSR      CHKNUM
053900030822     C                   IF        PiNum=*on
054000030822     C                   Z-ADD     PiVal         VABVLB
054100030822     C                   ENDIF
054200030822     C                   ENDIF
054300030822     C* QFT
054400030822     C                   IF        %subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):1:3)
054700030822     C                             = 'QFT'
054800030822     C                   EVAL      PiStr=%trim(%subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100030822     C                   EXSR      CHKNUM
055200030822     C                   IF        PiNum=*on
055300030822     C                   Z-ADD     PiVal         VABQFT
055400030822     C                   ENDIF
055500030822     C                   ENDIF
055600030822     C* CBO
055700030822     C                   IF        %subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):1:3)
056000030822     C                             = 'CBO'
056100030822     C                   EVAL      VABCBO=%trim(%subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400030822     C                   ENDIF
056500030822     C* TSP
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'TSP'
057000030822     C                   EVAL      VABTSP=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   ENDIF
057400030822     C* VAS
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'VAS'
057900030822     C                   EVAL      VABVAS=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   ENDIF
058300030822     C* VCA
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'VCA'
058800030822     C                   EVAL      VABVCA=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   ENDIF
059200030822     C* GCA
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'GCA'
059700030822     C                   EVAL      VABGCA=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   ENDIF
060100030822     C* CTM
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'CTM'
060600030822     C                   EVAL      VABCTM=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   ENDIF
061000030822     C* FFD
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'FFD'
061500030822     C                   EVAL      VABFFD=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   ENDIF
061900030822     C* VAD
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'VAD'
062400030822     C                   EVAL      VABVAD=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   ENDIF
062800030822     C* GMA
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'GMA'
063300030822     C                   EVAL      VABGMA=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* GGA
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'GGA'
064200030822     C                   EVAL      VABGGA=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* GVA
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'GVA'
065100030822     C                   EVAL      VABGVA=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* TC1
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'TC1'
066000030822     C                   EVAL      VABTC1=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* TC2
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'TC2'
066900030822     C                   EVAL      VABTC2=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* ...
067400030822     C                   ENDIF
067500030822     C                   ENDDO
067600020204     C*
067700010330     C                   ENDSR
067800010607     C*----------------------------------------------------*
067900101018     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
068000010607     C*----------------------------------------------------*
068100010607     C     IMPVAB        BEGSR
068200010607     C*
068300010607     C                   SETOFF                                       3132
068400030515     C*
068500010607     C                   MOVEL     datcor        VABAAS
068600010607     C                   MOVE      datcor        VABMGS
068700010607     C*
068800030715     C**********
068900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
069000030715     C**********
069100030715     C                   IF        wGiro = *zeros
069200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
069300031201     C     minu:maiu     XLATE     vindta        vindta
069400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
069500040324     C                   DOW       posDa <= %len(%trimr(vindta))
069600030715     C*
069700030715     C* Gestisco il 1° campo
069800030715     C                   IF        i = 1
069900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
070000030715     C                   EVAL      posDa = 1
070100030715     C* Eseguo lo scan x trovare la fine del primo campo
070200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
070300030715     C* A questo punto "estraggo" il campo corrente
070400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070500030715     C                                                    (posA-posDa))
070600030715     C* X i campi successivi al 1°
070700030715     C                   ELSE
070800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
070900030715     C                   EVAL      posDa = posA + 1
071000030715     C* Eseguo lo scan x trovare la fine del campo corrente
071100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071200030715     C* Gestisco l'ultimo campo
071300030715     C                   IF        posA = *zeros
071400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
071500030715     C* A questo punto "estraggo" il campo corrente
071600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
071700030715     C                                                    (posA-posDa))
071800030715     C                   LEAVE
071900030715     C                   ELSE
072000030715     C* A questo punto "estraggo" il campo corrente
072100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
072200030715     C                                                    (posA-posDa))
072300030715     C                   ENDIF
072400030715     C                   ENDIF
072500030715     C* Incremento il contatore d campo
072600030715     C                   EVAL      i = i +1
072700030715     C                   ENDDO
072800030715     C*
072900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
073000030715     C* delimitazione testo
073100030715     C                   EVAL      i = 1
073200030715     C                   DOW       i < %elem(SkSplitFLD)
073300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
073400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
073500030715     C                   EVAL      i = i + 1
073600030715     C                   ENDDO
073700030715     C                   EVAL      wGiro = 1
073800030715     C                   ELSE
073900030715     C                   EVAL      wGiro = 2
074000031204     C**********
074100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
074200031204     C* d separatore campo
074300031204     C**********
074400040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
074500040304     C                   EVAL      vindta = ' ' + vindta
074600040304     C                   ENDIF
074700040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
074800040304     C***                EVAL      vindta = %subst(vindta:2)
074900040304     C***                ENDDO
075000031204     C*
075100031204     C                   Z-ADD     *zeros        lunghInput        4 0
075200040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
075300040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
075400040304     C                   EVAL      vindta = vindta + ' '
075500040304     C                   ENDIF
075600040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
075700040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
075800040304     C***                EVAL      lunghInput = %len(%trim(vindta))
075900040304     C***                ENDDO
076000030715     C**********
076100030715     C* Effettuo lo split del campo dati d input
076200030715     C**********
076300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
076400040324     C                   DOW       posDa <= %len(%trimr(vindta))
076500030715     C*
076600030715     C* Gestisco il 1° campo
076700030715     C                   IF        i = 1
076800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
076900030715     C                   EVAL      posDa = 1
077000030715     C* Eseguo lo scan x trovare la fine del primo campo
077100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077200030715     C* A questo punto "estraggo" il campo corrente
077300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
077400030715     C                                                    (posA-posDa))
077500030715     C* X i campi successivi al 1°
077600030715     C                   ELSE
077700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
077800030715     C                   EVAL      posDa = posA + 1
077900031204     C* Verifico che nn vi sia il campo nn valorizzato
078000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
078100031204     C* Se campo nn valorizzato skippo al prossimo
078200031204     C                   EVAL      SkSplitCSV(i) = *blanks
078300031204     C                   EVAL      posA  = posA + 1
078400031204     C                   ELSE
078500030715     C* Eseguo lo scan x trovare la fine del campo corrente
078600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078700030715     C* Gestisco l'ultimo campo
078800030715     C                   IF        posA = *zeros
078900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
079000030715     C* A questo punto "estraggo" il campo corrente
079100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079200030715     C                                                    (posA-posDa))
079300030715     C                   LEAVE
079400030715     C                   ELSE
079500030715     C* A questo punto "estraggo" il campo corrente
079600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079700030715     C                                                    (posA-posDa))
079800030715     C                   ENDIF
079900031204     C                   ENDIF
080000030715     C                   ENDIF
080100030715     C* Incremento il contatore d campo
080200030715     C                   EVAL      i = i +1
080300030715     C                   ENDDO
080400030715     C*
080500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
080600030715     C* delimitazione testo
080700030715     C                   EVAL      i = 1
080800030715     C                   DOW       i < %elem(SkSplitCSV)
080900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
081000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
081100030715     C                   EVAL      i = i + 1
081200030715     C                   ENDDO
081300030715     C*
081400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
081500030715     C                   EVAL      i = 1
081600030715     C                   DOW       i < %elem(SkSplitFLD)
081700030715     C*
081800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
081900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
082000030715     C                             SkSplitCSV(i) <> *blanks
082100030721     C*** ATB
082200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
082300030721     C                   EVAL      VABATB = SkSplitCSV(i)
082400030721     C                   ENDIF
082500030715     C*** RSD
082600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
082700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
082800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
082900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
083000030715     C* ==
083100030715     C                   ENDIF
083200030721     C*** RD2
083300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
083400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
083500030721     C                   ENDIF
083600030715     C*** IND
083700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
083800030715     C                   EVAL      VABIND = SkSplitCSV(i)
083900030715     C                   ENDIF
084000030715     C*** LOD
084100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
084200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
084300030715     C                   ENDIF
084400030715     C*** PRD
084500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
084600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
084700030715     C                   ENDIF
084800030721     C*** NZD
084900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
085000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
085100030721     C                   ENDIF
085200030715     C*** RMA
085300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
085400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
085500030715     C                   ENDIF
085600030715     C*** NOT
085700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
085800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
085900030715     C                   ENDIF
086000030715     C*** NT2
086100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
086200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
086300030715     C                   ENDIF
086400030715     C*** VCA
086500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
086600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
086700030715     C                   ENDIF
086800030715     C*** VAS
086900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
087000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
087100030715     C                   ENDIF
087200030715     C*** FFD
087300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
087400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
087500030715     C                   ENDIF
087600030715     C*** CBO
087700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
087800030715     C                   EVAL      VABCBO = SkSplitCSV(i)
087900030715     C                   ENDIF
088000030721     C*** GC1
088100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
088200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
088300030721     C                   ENDIF
088400030721     C*** GC2
088500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
088600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
088700030721     C                   ENDIF
088800030721     C*** TSP
088900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
089000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
089100030721     C                   ENDIF
089200030721     C*** NAS
089300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
089400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
089500030721     C                   ENDIF
089600030721     C*** TIC
089700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
089800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
089900030721     C                   ENDIF
090000030721     C*** GCA
090100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
090200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
090300030721     C                   ENDIF
090400030721     C*** XCO
090500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
090600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
090700030721     C                   ENDIF
090800030721     C*** CTM
090900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
091000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
091100030721     C                   ENDIF
091200030721     C*** TCR
091300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
091400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
091500030721     C                   ENDIF
091600030721     C*** CTS
091700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
091800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
091900030721     C                   ENDIF
092000030721     C*** FTM
092100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
092200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
092300030721     C                   ENDIF
092400030721     C*** VAD
092500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
092600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
092700030721     C                   ENDIF
092800030721     C*** GMA
092900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
093000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
093100030721     C                   ENDIF
093200030721     C*** GGA
093300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
093400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
093500030721     C                   ENDIF
093600030721     C*** GVA
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
093800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030721     C*** TC1
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
094200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030721     C*** TC2
094500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
094600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
094700030721     C                   ENDIF
094800030721     C*** SCL
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
095000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030721     C*** RMO
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
095400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030721     C*** NMO
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
095800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*
096100070705     C* Reperisco quindi i campi numerici...
096200030721     C*** CAD
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
096400070705     C                   EVAL      VABCAD = SkSplitCSV(i)
096500030721     C                   ENDIF
096600030721     C*** RMN
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
096800030721     C                   EVAL      PiStr=SkSplitCSV(i)
096900030721     C                   EXSR      CHKNUM
097000030721     C                   IF        PiInt=*on
097100030721     C                   Z-ADD     PiVal         VABRMN
097200030721     C                   ELSE
097300030721     C                   SETON                                        32
097400030721     C                   EVAL      VABRMN = 1
097500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097600030721     C                             + ' ' + 'VABRMN'
097700030721     C                   ENDIF
097800030721     C                   ENDIF
097900030721     C*** NSP
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
098100030721     C                   EVAL      PiStr=SkSplitCSV(i)
098200030721     C                   EXSR      CHKNUM
098300100517     C                   IF        PiInt=*on
098400030721     C                   Z-ADD     PiVal         VABNSP
098500030721     C                   ELSE
098600030721     C                   SETON                                        32
098700030721     C                   EVAL      VABNSP = *zeros
098800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098900030721     C                             + ' ' + 'VABNSP'
099000030721     C                   ENDIF
099100030721     C                   ENDIF
099200030721     C*** NCL
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
099400030721     C                   EVAL      PiStr=SkSplitCSV(i)
099500030721     C                   EXSR      CHKNUM
099600030721     C                   IF        PiInt=*on
099700030721     C                   Z-ADD     PiVal         VABNCL
099800030721     C                   ELSE
099900030721     C                   SETON                                        32
100000030721     C                   EVAL      VABNCL = *zeros
100100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100200030721     C                             + ' ' + 'VABNCL'
100300030721     C                   ENDIF
100400030721     C                   ENDIF
100500030721     C*** PKB
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
100700030721     C                   EVAL      PiStr=SkSplitCSV(i)
100800030721     C                   EXSR      CHKNUM
100900030721     C                   IF        PiNum=*on
101000030721     C                   Z-ADD     PiVal         VABPKB
101100030721     C                   ELSE
101200030721     C                   SETON                                        32
101300030721     C                   EVAL      VABPKB = *zeros
101400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101500030721     C                             + ' ' + 'VABPKB'
101600030721     C                   ENDIF
101700030721     C                   ENDIF
101800030721     C*** CAS
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
102000030721     C                   EVAL      PiStr=SkSplitCSV(i)
102100030721     C                   EXSR      CHKNUM
102200030721     C                   IF        PiNum=*on
102300030721     C                   Z-ADD     PiVal         VABCAS
102400030721     C                   ELSE
102500030721     C                   SETON                                        32
102600030721     C                   EVAL      VABCAS = *zeros
102700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102800030721     C                             + ' ' + 'VABCAS'
102900030721     C                   ENDIF
103000030721     C                   ENDIF
103100030721     C*** CCM
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
103300030721     C                   EVAL      PiStr=SkSplitCSV(i)
103400030721     C                   EXSR      CHKNUM
103500030721     C                   IF        PiInt=*on      AND
103600030721     C                             PiVal<=9999999 AND
103700030721     C                             PiVal>*zeros
103800030721     C                   Z-ADD     PiVal         VABCCM
103900030721     C                   ELSE
104000030721     C                   SETON                                        32
104100030721     C                   EVAL      VABCCM = *zeros
104200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104300030721     C                             + ' ' + 'VABCCM'
104400030721     C                   ENDIF
104500030721     C                   ENDIF
104600030721     C*** LNP
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
104800030721     C                   EVAL      PiStr=SkSplitCSV(i)
104900030721     C                   EXSR      CHKNUM
105000030721     C                   IF        PiInt=*on    AND
105100030721     C                             PiVal<=999   AND
105200030721     C                             PiVal>*zeros
105300030721     C                   Z-ADD     PiVal         VABLNP
105400030721     C                   ELSE
105500030721     C                   SETON                                        32
105600030721     C                   EVAL      VABLNP = *zeros
105700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105800030721     C                             + ' ' + 'VABLNP'
105900030721     C                   ENDIF
106000030721     C                   ENDIF
106100030721     C*** NRS
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
106300030721     C                   EVAL      PiStr=SkSplitCSV(i)
106400030721     C                   EXSR      CHKNUM
106500030721     C                   IF        PiInt=*on AND
106600030721     C                             PiVal<=99
106700030721     C                   Z-ADD     PiVal         VABNRS
106800030721     C                   ELSE
106900030721     C                   SETON                                        32
107000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107100030721     C                             + ' ' + 'VABNRS'
107200030721     C                   ENDIF
107300030721     C                   ENDIF
107400030721     C*** LNA
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
107600030721     C                   EVAL      PiStr=SkSplitCSV(i)
107700030721     C                   EXSR      CHKNUM
107800030721     C                   IF        PiInt=*on    AND
107900030721     C                             PiVal<=999   AND
108000030721     C                             PiVal>*zeros
108100030721     C                   Z-ADD     PiVal         VABLNA
108200030721     C                   ELSE
108300030721     C                   SETON                                        32
108400030721     C                   EVAL      VABLNA = *zeros
108500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108600030721     C                             + ' ' + 'VABLNA'
108700030721     C                   ENDIF
108800030721     C                   ENDIF
108900030721     C*** CTR
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
109100030721     C                   EVAL      PiStr=SkSplitCSV(i)
109200030721     C                   EXSR      CHKNUM
109300030721     C                   IF        PiInt=*on  AND
109400030721     C                             PiVal<=999
109500030721     C                   Z-ADD     PiVal         VABCTR
109600030721     C                   ELSE
109700030721     C                   SETON                                        32
109800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109900030721     C                             + ' ' + 'VABCTR'
110000030721     C                   ENDIF
110100030721     C                   ENDIF
110200030721     C*** IAS
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
110400030721     C                   EVAL      PiStr=SkSplitCSV(i)
110500030721     C                   EXSR      CHKNUM
110600030721     C                   IF        PiNum=*on
110700030721     C                   Z-ADD     PiVal         VABIAS
110800030721     C                   ELSE
110900030721     C                   SETON                                        32
111000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111100030721     C                             + ' ' + 'VABIAS'
111200030721     C                   ENDIF
111300030721     C                   ENDIF
111400030721     C*** VLB
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
111600030721     C                   EVAL      PiStr=SkSplitCSV(i)
111700030721     C                   EXSR      CHKNUM
111800030721     C                   IF        PiNum=*on
111900030721     C                   Z-ADD     PiVal         VABVLB
112000030721     C                   ELSE
112100030721     C                   SETON                                        32
112200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112300030721     C                             + ' ' + 'VABVLB'
112400030721     C                   ENDIF
112500030721     C                   ENDIF
112600030721     C*** QFT
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
112800030721     C                   EVAL      PiStr=SkSplitCSV(i)
112900030721     C                   EXSR      CHKNUM
113000030721     C                   IF        PiNum=*on
113100030721     C                   Z-ADD     PiVal         VABQFT
113200030721     C                   ELSE
113300030721     C                   SETON                                        32
113400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113500030721     C                             + ' ' + 'VABQFT'
113600030721     C                   ENDIF
113700030721     C                   ENDIF
113800030721     C*** NCD
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
114000030721     C                   EVAL      PiStr=SkSplitCSV(i)
114100030721     C                   EXSR      CHKNUM
114200030721     C                   IF        PiInt=*on      AND
114300030721     C                             PiVal<=9999999
114400030721     C                   Z-ADD     PiVal         VABNCD
114500030721     C                   ELSE
114600030721     C                   SETON                                        32
114700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114800030721     C                             + ' ' + 'VABNCD'
114900030721     C                   ENDIF
115000030721     C                   ENDIF
115100030721     C*** NCA
115200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiInt=*on      AND
115600030721     C                             PiVal<=9999999
115700030721     C                   Z-ADD     PiVal         VABNCA
115800030721     C                   ELSE
115900030721     C                   SETON                                        32
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABNCA'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** ZNC
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiInt=*on AND
116900030721     C                             PiVal<=99
117000030721     C                   Z-ADD     PiVal         VABZNC
117100030721     C                   ELSE
117200030721     C                   SETON                                        32
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABZNC'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** DCR
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on       AND
118200030721     C                             PiVal<=99999999
118300030721     C                   Z-ADD     PiVal         VABDCR
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABDCR'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** HCR
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiInt=*on   AND
119500030721     C                             PiVal<=9999
119600030721     C                   Z-ADD     PiVal         VABHCR
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000030721     C                             + ' ' + 'VABHCR'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** VMD
120400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
120500030721     C                   EVAL      PiStr=SkSplitCSV(i)
120600030721     C                   EXSR      CHKNUM
120700030721     C                   IF        PiNum=*on
120800030721     C                   Z-ADD     PiVal         VABVMD
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABVMD'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** ANT
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on        AND
122000030721     C                             PiVal<=999999999
122100030721     C                   Z-ADD     PiVal         VABANT
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500030721     C                             + ' ' + 'VABANT'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800030721     C*** CMO
122900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
123000030721     C                   EVAL      PiStr=SkSplitCSV(i)
123100030721     C                   EXSR      CHKNUM
123200030721     C                   IF        PiInt=*on
123300030721     C                   Z-ADD     PiVal         Num5_0
123400030721     C                   MOVEL(p)  Num5_0        VABCMO
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      VABCMO = *zeros
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABCMO'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030715     C*
124300030715     C                   ENDIF
124400030715     C                   EVAL      i = i + 1
124500030715     C                   ENDDO
124600070705     C*
124700070705     C* Considerazioni particolari
124800070705     C*** NZD
124900070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
125000070705     C                   EVAL      VABNZD = *blanks
125100070705     C                   ENDIF
125200070705     C*** CAD
125300070705     C                   IF        VABNZD = *blanks
125400070705     C                   EVAL      PiStr=VABCAD
125500070705     C                   EXSR      CHKNUM
125600070705     C                   IF        PiInt=*on
125700070705     C                   Z-ADD     PiVal         Num5_0
125800070705     C                   MOVEL(p)  Num5_0        VABCAD
125900070705     C                   ELSE
126000070705     C                   SETON                                        32
126100070705     C                   EVAL      VABCAD = *zeros
126200070705     C                   EVAL      vinmsg = %trimr(vinmsg)
126300070705     C                             + ' ' + 'VABCAD'
126400070705     C                   ENDIF
126500070705     C                   ENDIF
126600080222     C*
126700080222     C* Se provincia nn valorizzata la reperisco
126800080222     C* tramite TISI95R a seconda dei dati d instradamento presenti
126900080222     C                   IF        VABPRD = *blanks
127000080222     C                   CLEAR                   TISI95DS
127100080222     C                   EVAL      I95TCN = '3'
127200080222     C                   Z-ADD     datcor        I95DAT
127300080222     C                   EVAL      I95NAR = VABNZD
127400080222     C                   EVAL      I95CAP = VABCAD
127500080222     C                   EVAL      I95LOC = VABLOD
127600080222     C                   CALL      'TISI95R'
127700080222     C                   PARM                    TISI95DS
127800080222     C                   EVAL      VABPRD = O95PRV
127900080222     C                   ENDIF
128000020204     C*
128100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
128200020204     C                   EXSR      CHKIMPDIV
128300030715     C                   ENDIF
128400020204     C*
128500010607     C                   ENDSR
128600010607     C*----------------------------------------------------*
128700010601
128800020204
128900020204     C*----------------------------------------------------*
129000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
129100020204     C*----------------------------------------------------*
129200020204     C     CHKIMPDIV     BEGSR
129300060524     C*
129400060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
129500060524     C                   IF        vabCAS > *zeros
129600060524     C                   IF        vabCBO = '1'
129700060524     C                   EVAL      vabCBO = '4'
129800060524     C                   ENDIF
129900060524     C                   IF        vabCBO = '2'
130000060524     C                   EVAL      vabCBO = '6'
130100060524     C                   ENDIF
130200060524     C                   ENDIF
130300020204     C*
130400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
130500020204     C                   Z-ADD     *zeros        wrkDec            9 9
130600020204     C*
130700020204     C* Come prima cosa effettuo considerazioni sulla divisa
130800020204     C                   IF        vabIAS > *zeros
130900020204     C                   IF        vabVAS <> 'EUR'
131000020204     C                   EVAL      vabVAS =  'ITL'
131100020204     C                   ENDIF
131200020204     C                   ENDIF
131300020204     C*
131400020204     C                   IF        vabCAS > *zeros
131500020204     C                   IF        vabVCA <> 'EUR'
131600020204     C                   EVAL      vabVCA =  'ITL'
131700020204     C                   ENDIF
131800020204     C                   ENDIF
131900020204     C*
132000020204     C                   IF        vabVMD > *zeros
132100020204     C                   IF        vabVAD <> 'EUR'
132200020204     C                   EVAL      vabVAD =  'ITL'
132300020204     C                   ENDIF
132400020204     C                   ENDIF
132500020204     C*
132600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
132700020204     C                   Z-ADD     vabIAS        wrkDec
132800020204     C                   IF        wrkDec > *zeros
132900020204     C                   IF        vabVAS = 'ITL'
133000020204     C                   EVAL      vabIAS = *zeros
133100020204     C                   ENDIF
133200020204     C                   ENDIF
133300020204     C*
133400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
133500020204     C                   Z-ADD     vabCAS        wrkDec
133600020204     C                   IF        wrkDec > *zeros
133700020204     C                   IF        vabVCA = 'ITL'
133800020204     C                   EVAL      vabCAS = *zeros
133900020204     C                   ENDIF
134000020204     C                   ENDIF
134100020204     C*
134200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
134300020204     C                   Z-ADD     vabVMD        wrkDec
134400020204     C                   IF        wrkDec > *zeros
134500020204     C                   IF        vabVAD = 'ITL'
134600020204     C                   EVAL      vabVMD = *zeros
134700020204     C                   ENDIF
134800020204     C                   ENDIF
134900020204     C*
135000020204     C                   ENDSR
135100020204     C***
135200020204
135300010330
135400010330
135500010330     C*----------------------------------------------------*
135600010330     C*  CONTROLLO NUMERICITA' CAMPI
135700010330     C*----------------------------------------------------*
135800010330     C     CHKNUM        BEGSR
135900010330     C*
136000010606     C                   IF        PiDecChr = *blanks
136100030715     C                   EVAL      PiDecChr = CharNUM
136200010606     C                   ENDIF
136300010606     C*
136400010606     C                   CALL(e)   'ISNUMERIC'
136500010330     C                   PARM                    PiStr            30
136600010606     C                   PARM                    PiDecChr          1
136700010330     C                   PARM      *ZEROS        PiVal            30 9
136800010330     C                   PARM      '0'           PiInt             1
136900010330     C                   PARM      '0'           PiNum             1
137000010330     C                   IF        %error
137100010606     C                   EVAL      PiNum=*off
137200010330     C                   ENDIF
137300010330     C*
137400010330     C                   ENDSR
137500010330     C***
137600010330
137700010601
137800010601
137900010601
138000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
138100010601     C     repfil        BEGSR
138200010601     C*
138300010601     C                   if        invfil = *zeros and
138400010601     C                             depfil > *zeros and
138500010629     C                             (vinflg = *blanks or
138600010629     C                              vinflg = *zeros)
138700010601     C
138800010601     C                   eval      invfil = depfil
138900010601     C                   endif
139000010601     C*
139100010601     C                   if        depfil <> invfil and
139200010601     C                             invfil > *zeros
139300010601     C                   eval      flgMulti = '1'
139400010601     C                   if        vinflg = *blanks
139500010601     C                   add       1             cntNonEl
139600010601     C                   endif
139700010601     C                   endif
139800010601     C*
139900010601     C                   if        vinflg = '2'
140000010601     C                   eval      flgStato = '2'
140100010601     C                   endif
140200010601     C*
140300010601     C                   ENDSR
140400010601     C***
140500010601
140600010601
140700010601
140800010330
140900010330
141000010330
141100990920      /TITLE Invio dei dati al punto operativo.
141200000613     C     invio         BEGSR
141300990920     C*
141400990920     C                   reset                   dscmz
141500010601     C                   move      invfil        cmzdst
141600990920     C                   eval      cmzfld = vlrfou
141700990920     C                   eval      cmzmbd = vlrhdl
141800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
141900000710     C                   if        prmfir = *blanks
142000101018     C                   eval      cmzfla = 'EDIVAB0F'
142100101018     C                   eval      cmzmba = 'EDIVAB0F'
142200000710     C                   else
142300000710     C                   eval      cmzfla = prmfir
142400000710     C                   eval      cmzmba = prmfir
142500000710     C                   endif
142600990920     C                   eval      cmznrr = *zeros
142700990920     C                   move      §ctrok        cmznrr
142800021018     C                   eval      cmzlba = vlrfl1
142900990920     C                   call(e)   'TIS711C'
143000990920     C                   parm                    dscmz
143100990921     C                   parm      *blanks       esito
143200990920     C                   if        %error
143300990920     C                             or cmzerr = '1'
143400990921     C                             or esito  = '1'
143500000710     C                   eval      wrkesito = '3'
143600990920     C                   endif
143700990920     C*
143800000613     C                   ENDSR
143900990910
144000010601
144100010601
144200010601
144300010601
144400010601      /TITLE Invio dei dati al punto operativo.
144500010601     C     opeini        BEGSR
144600010601     C*
144700010601     C* Inizializzo flag e contatori operativi
144800010601     C                   movel     '0'           flgGiro           1
144900010601     C                   movel     '0'           flgMulti          1
145000010601     C                   movel     '1'           flgStato          1
145100010615     C                   movel     '0'           flgOk             1
145200010601     C                   z-add     *zeros        cntNonEl         10 0
145300010601     C                   z-add     *zeros        depfil            3 0
145400010601     C                   z-add     *zeros        invfil            3 0
145500010601     C*
145600010601     C                   ENDSR
145700010601     C***
145800050421
145900050421
146000050421
146100050421     C     *pssr         BEGSR
146200050421     C*
146300050421     C                   eval      wrkesito = '2'
146400050421     C*
146500050421     C                   ENDSR     '*CANCL'
146600050421     C***
146700100419
146800100419
146900100419
147000100419     C     normCSV       BEGSR
147100100419     C*
147200100419     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
147300100419     C* separatori campi
147400100419     C                   if        CharTXT <> *blanks
147500100419     C                   z-add     *zeros        wLenVINDTA        4 0
147600100419     C                   z-add     *zeros        wFlgVINDTA        1 0
147700100419     C                   setoff                                       80
147800100419     C                   dow       wLenVINDTA < %len(%trim(vindta))
147900100419     C                   eval      wLenVINDTA = wLenVINDTA + 1
148000100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
148100100419     C                   if        not *in80
148200100419     C                   seton                                        80
148300100419     C                   else
148400100419     C                   setoff                                       80
148500100419     C                   endif
148600100419     C                   endif
148700100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
148800100419     C                             and *in80 = *on
148900100419     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
149000100419     C                   endif
149100100419     C                   enddo
149200100419     C                   endif
149300100419     C*
149400100419     C                   ENDSR
149500100419     C***
149600010601
149700010330
149800010330
149900000613     C     *inzsr        BEGSR
150000990910     C*
150100990910     C     *entry        plist
150200990920     C                   parm                    tivlrds
150300990921     C                   parm      wrkesito      esito
150400000724     C                   parm                    prmlit
150500000710     C                   parm                    prmfir
150600101018     C*
150700101018     C* Determino il CMR
150800101018     C                   movel     *blanks       wCMR             35
150900101018     C                   movel     datcor        wDataOraAlfa     14
151000101018     C                   move      oracor        wDataOraAlfa
151100101018     C                   eval      wCMR = 'FNVAB_' + wDataOraAlfa
151200010330     C*
151300010330     C* CALCOLA LA DATA CORRENTE
151400010330     C                   time                    wn14             14 0
151500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
151600101018     C                   movel     wn14          oracor            6 0          *ORA
151700010330     C                   z-add     wn8           g08dat
151800010330     C                   z-add     *zeros        g08inv
151900010330     C                   movel     '0'           g08err
152000010330     C                   call      'XSRDA8'
152100010330     C                   parm                    wlbda8
152200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
152300000613     C*
152400000613     C                   ENDSR
152500000613     C***
152600990908
152700030715     Otitvxlp   E            riepilogo         2
152800990915     O                                              'Upload via Internet'
152900990915     O                                           +1 'Traduzione TIVIN00R -'
153000101018     O                                           +1 'EDIVABWR'
153100990915     O                                           +1 'Testate bolle'
153200990915     O          E            riepilogo   2
153300990915     O                       wrkdata
153400990915     O                       wrkora              +1
153500990915     O                       procname            +1
153600990915     O          E            riepilogo   2
153700990915     O                                              'Cliente..................:'
153800010605     O                       VABCCM        z     +1
153900990915     O          E            riepilogo   2
154000990920     O                                              'Riferimento Strategi.....:'
154100990920     O                       vlrhdl              +1
154200990915     O          E            riepilogo   2
154300990915     O                                              'Giusti...................:'
154400971022     O                       §CTROK        2   +  1
154500990915     O          E            riepilogo   2
154600990915     O                                              'Sbagliati e corretti.....:'
154700971022     O                       §CTRMO        2   +  1
154800990915     O          E            riepilogo   2
154900990915     O                                              'Sbagliati e scartati.....:'
155000971022     O                       §CTRNO        2   +  1
155100000613
155200030715     Otitvxlps  E            testdett          2
155300000613     O                                              'Upload via Internet'
155400000613     O                                           +1 'Traduzione TIVIN00R -'
155500101018     O                                           +1 'EDIVABWR'
155600000613     O                                           +1 'Testate bolle'
155700000616     O          E            testdett    3
155800000613     O                                           +2 'N° rec'
155900000613     O                                           +3 'Anteprima contenuto'
156000000616     O          E            rigadett    2
156100000613     O                       rrnum               +2
156200000621     O                       recko               +3
156300000616     O          E            findett     2
156400000613     O                       wrkdata
156500000613     O                       wrkora              +1
156600000613     O                       procname            +1
156700000616     O          E            findett     2
156800000613     O                                              'Cliente..................:'
156900010605     O                       VABCCM        z     +1
157000000616     O          E            findett     2
157100000613     O                                              'Riferimento Strategi.....:'
157200000613     O                       vlrhdl              +1
157300000616     O          E            findett     2
157400000613     O                                              'Giusti...................:'
157500000613     O                       §CTROK        2   +  1
157600000616     O          E            findett     2
157700000613     O                                              'Sbagliati e corretti.....:'
157800000613     O                       §CTRMO        2   +  1
157900000616     O          E            findett     2
158000000613     O                                              'Sbagliati e scartati.....:'
158100000613     O                       §CTRNO        2   +  1
158200000613** CMD - COMANDI CL
158300030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
158400030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
158500030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
158600010606
158700000613
